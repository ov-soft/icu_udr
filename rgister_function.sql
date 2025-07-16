create function normalize (
      in_str varchar(8191),
      translit_diacritics boolean = true,
      clear_parentheses boolean = false,
      sort_words boolean = false,
      remove_delimiters boolean = true,
      replace_repeats boolean = true,
      replace_brackets boolean = true,
      ascii_locase boolean = true,
      ascii_capitalize boolean = false,
      rome2arabian boolean = false,
      delete_spaces boolean = false,
      replace_y2i boolean = true,
      replace_i2y boolean = false,
      non_ascii2hex boolean = false)
  returns varchar(8191) external name 'icu_udr!UTF8_normalize' engine udr
  as
  'Any-NFKC;Any-Latin;Latin-ASCII';
