//
//  docopt_util.h
//  docopt
//
//  Created by Jared Grubb on 2013-11-04.
//  Copyright (c) 2013 Jared Grubb. All rights reserved.
//

#ifndef docopt_docopt_util_h
#define docopt_docopt_util_h

#if DOCTOPT_USE_BOOST_REGEX
#include <boost/regex.hpp>
namespace std {
    using boost::regex;
    using boost::sregex_token_iterator;
}
#else
#include <regex>
#endif

#pragma mark -
#pragma mark General utility

#include <algorithm>
#include <array>
#include <exception>
#include <initializer_list>
#include <limits>
#include <cstring>

namespace {
	class string_view {
	public:
		typedef std::initializer_list<const char> char_list;
		typedef std::string::const_iterator iterator;
		typedef std::pair<string_view, string_view> pair;
		typedef std::vector<string_view> vector;

		string_view(){}

		string_view(const char *str):
			_begin(str),
			_end(str+strlen(str))
		{}

		string_view(const std::string &str):
			_begin(std::begin(str)),
			_end(std::end(str))
		{}

		string_view(iterator begin, iterator end):
			_begin(begin),
			_end(end)
		{}

		iterator begin() const { return _begin; }
		iterator end()   const { return _end;   }

		const char &at(size_t i) const {
			if (i >= std::distance(_begin,_end))
				throw std::out_of_range("The provided index lies outside of the range's length.");
			return (*this)[i];
		}
		const char &operator[](size_t i) const { return *(_begin+i); }

		template<class it>
		size_t find(it begin, it end, size_t pos = 0) const {
			auto i = _begin + pos;
			if (i >= _end) return std::string::npos;

			i = std::find_if(i,_end,[&](char c) {
					return end != std::find(begin,end,c);
				});

			if (i == _end) return std::string::npos;
			else return std::distance(_begin,i);
		}

		size_t find(string_view str, size_t pos = 0) const {
			auto i = _begin + pos;
			if (i >= _end) return std::string::npos;
			i = std::search(str._begin,str._end,_begin,_end);
			if (i == _end) return std::string::npos;
			else return std::distance(_begin,i);
		}

		size_t find(const char_list &chars, size_t pos = 0) const {
			return find(chars.begin(),chars.end(),pos);
		}

		size_t find(char c, size_t pos = 0) const {
			return find({c},pos);
		}

		bool empty() const { return _begin == _end; }
		size_t size() const { return std::distance(_begin,_end); }

		pair split_once_before(string_view str) const {
			if (str.empty() || empty()) return { *this, {} };
			auto i = std::search(_begin,_end,str.begin(),str.end());
			return { { _begin, i }, { i, _end } };
		}

		pair split_once_after(string_view str) const {
			auto result = split_once_before(str);
			if (!result.second.empty()) {
				result.first._end += str.size();
				result.second._begin += str.size();
			}
			return result;
		}

		template<class it>
		pair split_once_before(it begin, it end) const {
			if (begin == end) return { *this, {} };
			auto i = std::find_if(_begin,_end,[&](char c) {
						return end != std::find(begin,end,c);
					});
			return { { _begin, i }, { i, _end } };
		}

		template<class it>
		pair split_once_after(it begin, it end) const {
			auto result = split_once_before(begin,end);
			if (!result.second.empty()) {
				++result.first._end;
				++result.second._begin;
			}
			return result;
		}

		pair split_once_before(const char_list &chars) const {
			return split_once_before(std::begin(chars),std::end(chars));
		}

		pair split_once_after(const char_list &chars) const {
			return split_once_after(std::begin(chars),std::end(chars));
		}

		pair split_once_before(char c) const {
			return split_once_before({ c });
		}

		pair split_once_after(char c) const {
			return split_once_after({ c });
		}

		vector split_after(string_view str, size_t n = 0) const {
			if (str.empty() || empty()) return { *this };
			if (n == 0) n = std::numeric_limits<size_t>::max();
			vector result;
			string_view segment;
			string_view source(_begin,_end);
			while(n > 0) {
				std::tie(segment,source) = source.split_once_after(str);
				result.emplace_back(segment);
				if (source.size() == 0) break;
			}
			if (n > 0 && result.back().ends_with(str))
				result.emplace_back();
			return result;
		}

		vector split_before(string_view str, size_t n = 0) const {
			if (str.empty() || empty()) return { *this };
			auto result = split_after(str,n);
			auto delta = str.size();
			for (size_t i = 1; i < result.size(); ++i) {
				result[i-1]._end -= delta;
				result[i]._begin -= delta;
			}
			return result;
		}

		template<class it>
		vector split_after(it begin, it end, size_t n = 0) const {
			if (begin == end || empty()) return { *this };
			if (n == 0) n = std::numeric_limits<size_t>::max();
			vector result;
			string_view segment;
			string_view source(_begin,_end);
			while(n > 0) {
				std::tie(segment,source) = source.split_once_after(begin,end);
				result.emplace_back(segment);
				if (source.size() == 0) break;
				--n;
			}
			if (n > 0 && std::any_of(begin,end,[&](char c){ return result.back().ends_with(c); }))
				result.emplace_back();
			return result;
		}

		template<class it>
		vector split_before(it begin, it end, size_t n = 0) const {
			if (begin == end || empty()) return { *this };
			auto result = split_after(begin,end,n);
			for (size_t i = 1; i < result.size(); ++i) {
				--result[i-1]._end;
				--result[i]._begin;
			}
			return result;
		}

		vector split_before(const char_list &chars, size_t n = 0) const {
			return split_before(std::begin(chars), std::end(chars), n);
		}

		vector split_after(const char_list &chars, size_t n = 0) const {
			return split_after(std::begin(chars), std::end(chars), n);
		}

		vector split_before(char c, size_t n = 0) const {
			return split_before({ c }, n);
		}

		vector split_after(char c, size_t n = 0) const {
			return split_after({ c }, n);
		}

		template<class it>
		string_view strip(it begin, it end) const {
			if (empty()) return {};
			auto i = std::find_if_not(_begin,_end,[&](char c) {
					return std::find(begin,end,c) != end;
			});
			return {i,_end};
		}

		string_view strip(const char_list &chars) const {
			return strip(chars.begin(),chars.end());
		}

		string_view strip(char c) const {
			std::array<char,1> chars { c };
			return strip(chars.begin(),chars.end());
		}

		template<class it>
		string_view rstrip(it begin, it end) const {
			if (empty()) return {};
			auto i = _end-1;
			auto rend = _begin;
			for (; i >= rend; --i)
				if (std::find(begin,end,*i) == end)
					break;
			return {_begin,i+1};
		}

		string_view rstrip(const char_list &chars) const {
			return rstrip(chars.begin(),chars.end());
		}

		string_view rstrip(char c) const {
			std::array<char,1> chars { c };
			return rstrip(chars.begin(),chars.end());
		}

		bool starts_with(string_view str) const {
			auto i = str._begin;
			auto j = _begin;
			while(true) {
				if (i == str._end) return true;
				if (j == _end) return false;
				if (*i != *j) return false;
				++i; ++j;
			}
		}

		bool starts_with(char c) const {
			if (_begin == _end) return false;
			return *_begin == c;
		}

		bool ends_with(string_view str) const {
			auto i = str._end-1;
			auto j = _end-1;
			while(true) {
				if (i < str._begin) return true;
				if (j < _begin) return false;
				if (*i != *j) return false;
				--i; --j;
			}
		}

		bool ends_with(char c) const {
			if (_begin == _end) return false;
			return *(_end-1) == c;
		}

		template<class it>
		string_view trim(it begin, it end) const {
			return strip(begin,end).rstrip(begin,end);
		}

		string_view trim(const char_list &chars) const {
			return trim(chars.begin(),chars.end());
		}

		string_view trim(char c) const {
			return strip(c).rstrip(c);
		}

		operator std::string() const {
			return std::string(_begin,_end);
		}

		std::string str() const { return std::string(*this); }

		bool operator==(string_view other) {
			if (_begin == other._begin &&
				_end == other._end)
				return true;

			if (size() != other.size())
				return false;

			return std::equal(_begin,_end,other._begin);
		}

		bool operator!=(string_view other) {
			return !((*this)==other);
		}

	private:
		iterator _begin;
		iterator _end;
	};

	bool operator==(const std::string &a, string_view b) { return string_view(a) == b; }
	bool operator!=(const std::string &a, string_view b) { return string_view(a) != b; }

	bool starts_with(std::string const& str, std::string const& prefix)
	{
		if (str.length() < prefix.length()) return false;
		return std::equal(prefix.begin(), prefix.end(), str.begin());
	}

	std::string trim(std::string&& str,
			 const std::string& whitespace = " \t\n")
	{
		const auto strEnd = str.find_last_not_of(whitespace);
		if (strEnd==std::string::npos) return {}; // no content
		str.erase(strEnd+1);

		const auto strBegin = str.find_first_not_of(whitespace);
		str.erase(0, strBegin);

		return std::move(str);
	}

	std::vector<std::string> split(std::string const& str, size_t pos = 0)
	{
		const char* const anySpace = " \t\r\n\v\f";

		std::vector<std::string> ret;
		while (pos != std::string::npos) {
			auto start = str.find_first_not_of(anySpace, pos);
			if (start == std::string::npos) break;

			auto end = str.find_first_of(anySpace, start);
			auto size = end==std::string::npos ? end : end-start;
			ret.emplace_back(str.substr(start, size));

			pos = end;
		}

		return ret;
	}

	std::tuple<std::string, std::string, std::string> partition(std::string str, std::string const& point)
	{
		std::tuple<std::string, std::string, std::string> ret;

		auto i = str.find(point);

		if (i == std::string::npos) {
			// no match: string goes in 0th spot only
		} else {
			std::get<2>(ret) = str.substr(i + point.size());
			std::get<1>(ret) = point;
			str.resize(i);
		}
		std::get<0>(ret) = std::move(str);

		return ret;
	}

	template <typename I>
	std::string join(I iter, I end, std::string const& delim) {
		if (iter==end)
			return {};

		std::string ret = *iter;
		for(++iter; iter!=end; ++iter) {
			ret.append(delim);
			ret.append(*iter);
		}
		return ret;
	}

	std::vector<std::string> regex_split(std::string const& text, std::regex const& re)
	{
		std::vector<std::string> ret;
		for (auto it = std::sregex_token_iterator(text.begin(), text.end(), re, -1);
			it != std::sregex_token_iterator();
			++it) {
			ret.emplace_back(*it);
		}
		return ret;
	}
}

namespace docopt {
	template <class T>
	inline void hash_combine(std::size_t& seed, T const& v)
	{
		// stolen from boost::hash_combine
		std::hash<T> hasher;
		seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
	}
}

#endif
