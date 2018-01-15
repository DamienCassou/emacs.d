#!/bin/sh

old_clipboard_content=$(xclip -sel clip -o | sed 's/[\"]/\\&/g')

while sleep 2; do
    new_clipboard_content=$(xclip -sel clip -o | sed 's/[\"]/\\&/g')
    if [ "${old_clipboard_content}" != "${new_clipboard_content}" ]; then
        emacsclient -e "(kill-new \"${new_clipboard_content}\")"
        old_clipboard_content=${new_clipboard_content}
    fi
done
