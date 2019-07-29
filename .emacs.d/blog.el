(require 'ox-publish)

(setq org-publish-project-alist
      '(("blog"
         :base-directory "~/src/blog/org"
         :base-extension "org"
         :publishing-directory "~/src/blog/html"
         :recursive t
         :with-toc nil
         :section-numbers nil
         :time-stamp-file nil
         :html-doctype "html5"
         :html-validation-link nil
         :html-head
         "
<style type='text/css'>
body { width: 760px; max-width: 95%; margin: auto; padding-bottom: 5em; }
pre.src:hover:before { display: none; }
pre.src { padding-top: 8pt; }
pre { box-shadow: none; }
</style>"
         :publishing-function org-html-publish-to-html)))

(defun publish-blog ()
  (interactive)
  (org-publish-project "blog" t))
