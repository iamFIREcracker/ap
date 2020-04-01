(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :mkstr
               :with-gensyms)
  :package "AP.QUICKUTILS")

