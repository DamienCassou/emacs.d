;;; Compiled snippets and support files for `pillar-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'pillar-mode
                     '(("picture" "+${1:A caption}>file://`(file-relative-name (read-file-name \"Select a picture file \" nil \"figures/picture.png\" 'confirm-after-completion))`${2:|label=${3:figName}}${4:|width=${5:50}}+\n" "picture" nil nil nil nil nil nil)
                       ("[[[" "[[[${1:language=${2:$$(yas-choose-value '(\"Smalltalk\" \"HTML\" \"HTTP\" \"Bash\"))}}${3:|label=${4:script1}}${5:|caption=${6:A description of this script}}\n$0\n]]]" "scripts" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Jan 30 14:55:59 2014
