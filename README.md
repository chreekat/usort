# usort

## Synopses

1.
  ```
  $ usort list.txt
  ```
2.
  ```
  $ usort
  <paste in content>
  <c-d>
  ```

## About

Prioritizing task lists or backlogs is an important part of being organized.
Even when it isn't strictly required, I find thinking about priorities to be a
useful brainstorming device. But what's the best way to do it?

Staring at a drag-and-drop list and moving things at random always feels like
a futile, ineffecient exercise. Have I thought about this thing yet? Wait, did
I mean to put it above or *below* this other thing? Didn't I already decide on
that?

It seems especially silly since sorting a list is Computer Science 101. We've
got algorithms for that! We just need to use one of those algorithms to
efficiently choose items to be compared, and then do the comparisons ourselves.

And that's what usort is. Give it a list of items and it will prompt you to
compare them, one comparison at a time. At the end, it will print out your list.
Sorted! *rimshot*

### insort

Version 0.3 introduces insort, for sorting top-level [vimin](chreekat/vimin)
items using standard alphabetic comparison.

## History

This utility started as a [simple script] using a standard Perl function,
[sort], that is parameterized over the comparator function. That worked for
years, and only got replaced when I wanted the ability to **edit** items in
place, **delete** them, or **undo** previous actions. More features are slowly
being added over time.

See the [CHANGELOG].

[simple script]: https://github.com/chreekat/bscripts/blob/3f3e6c69bab951ad5d54f9f34e20dd90dfe4b1e7/bryansort
[sort]: http://perldoc.perl.org/functions/sort.html
