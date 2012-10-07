Emacs notmuch-labeler
==========

notmuch-labeler improves notmuch way of displaying labels through
fonts, pictures, and hyperlinks.

By default notmuch presents email labels as plain text. This
package improves notmuch by lettings users choose how to present
each label (e.g., with a special font, with a picture, ...).
Additionally, this package transforms each label into an hyperlink
to show all emails with this label.

![notmuch-labeler in a notmuch-search buffer](https://raw.github.com/DamienCassou/notmuch-labeler/master/doc/notmuch-search.png)

![notmuch-labeler in a notmuch-show buffer](https://raw.github.com/DamienCassou/notmuch-labeler/master/doc/notmuch-show.png)


# Installation and configuration

To configure this package, add the following to your init file:

    (require 'notmuch-labeler)

Then, you will get hyperlinks on all your labels.

Then, to rename the label "unread" to "new" and change the label's
color to blue, add the following to your init file:

    (notmuch-labeler-rename "unread" "new" ':foreground "blue")

You can replace the label "important" by a tag picture with the
following:

    (notmuch-labeler-image-tag "important")

You can also use your own picture with the following:

    (notmuch-labeler-image "important" "/path/to/picture.svg" 'svg)

Finally, you can hide the label "unread" with this code:

    (notmuch-labeler-hide "unread")


Enjoy.
