function fish_right_prompt --description "formats the right side of the prompt"
  set_color 555
  set time_str $CMD_DURATION_STR
  echo -e $time_str
  set_color normal
end