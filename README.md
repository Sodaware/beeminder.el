```
 _                             _             _                       _ 
| |                           (_)           | |                     | |
| |__    ___   ___  _ __ ___   _  _ __    __| |  ___  _ __      ___ | |
| '_ \  / _ \ / _ \| '_ ` _ \ | || '_ \  / _` | / _ \| '__|    / _ \| |
| |_) ||  __/|  __/| | | | | || || | | || (_| ||  __/| |    _ |  __/| |
|_.__/  \___| \___||_| |_| |_||_||_| |_| \__,_| \___||_|   (_) \___||_|
```

[![Melpa Status](http://melpa.org/packages/beeminder-badge.svg)](http://melpa.org/#/beeminder)
[![Melpa Stable Status](http://stable.melpa.org/packages/beeminder-badge.svg)](http://stable.melpa.org/#/beeminder)


A simple Emacs extension for working with [Beeminder](https://www.beeminder.com).

**beeminder.el** is designed to integrate with org-mode, and adds support for
submitting data directly to Beeminder when tasks are closed (see usage for some
examples).

Thanks to Sacha Chua for providing the inspiration and initial code for
this extension: <https://gist.github.com/sachac/9700455>


## Installation

It is recommended to install this package directly from
[MELPA](http://melpa.org/) or via
[el-get](http://www.emacswiki.org/emacs/el-get).

Alternatively you can install manually.  All Emacs installs are a little
different, but the basic outline is this:

  - Download the source code and put it somewhere Emacs can find it
    (probably `~/.emacs.d/`)
  - Add that directory to your `load-path` if it's not yet there:
    `(add-to-list 'load-path "/path/to/dir")`


## Configuration

  - Add `(require 'beeminder)` somewhere in your `~/.emacs.d/init.el`
  - Set your API authorization token and Beeminder username configuration
    variables.  You can find your token by logging into Beeminder and then
    visiting the following URI: <https://www.beeminder.com/api/v1/auth_token.json>
    To set it in Emacs, add the following code (or use `M-x customize`):

```lisp
(setq beeminder-username "username")
(setq beeminder-auth-token "token")
```

  - Set up your keyboard shortcuts.  Here are some recommendations:

```lisp
(global-set-key "\C-cba" 'beeminder-add-data)
(global-set-key "\C-cbw" 'beeminder-whoami)
(global-set-key "\C-cbg" 'beeminder-my-goals-org)
(global-set-key "\C-cbr" 'beeminder-refresh-goal)
(global-set-key "\C-cbt" 'beeminder-submit-clocked-time)
```

The recommended settings above will add the following keyboard shortcuts:

`C-c b a` (`beeminder-add-data`) - Add data to a Beeminder goal.  Prompts for the
goal identifier, a numeric value and an optional comment.

`C-c b g` (`beeminder-my-goals-org`) - Fetches all of your goals from Beeminder
and inserts them as a list of org-mode headlines.

`C-c b w` (`beeminder-whoami`) - Fetches your username from Beeminder.

`C-c b t` (`beeminder-submit-clocked-time`) - Submits clocked time for the
current goal (and any of its sub-tasks).


## Usage

### Interactive functions

#### `beeminder-add-data`

Directly add data to a beeminder goal.

#### `beeminder-my-goals-org`

Fetches all goals for the current user (set via `beeminder-username`) and
inserts them as a list of org-mode headings.

#### `beeminder-refresh-goal`

Fetches goal data for the current headline and updates its properties and
deadlines.

If the `beeminder-skip-deadlines` property is set to any value (such as "true"),
the org deadline will not be updated.

#### `beeminder-submit-clocked-time`

Submit all clocked time for the current goal (and any of its sub-tasks).  Only
time clocked since the `updated-at` property is counted.  Submits the number of
minutes clocked as the value.  If the goal property `beeminder-unit` is set to
"hours", it will submit the number of hours worked instead.

#### `beeminder-whoami`

Fetches the username associated with the current token (set via
`beeminder-auth-token`).  Not really useful, but good for checking if your
authorization token is set and valid.


### Integrating with org-mode

**beeminder.el** listens for changes to the state of org-mode todo items, and
can automatically add data to Beeminder once a task is complete.  In order to do
this, the task (or its ancestors) must have a `beeminder` property set to the
name of the goal to add to.

By default, closing a task will submit a value of "1" to the appropriate goal.
This behaviour can be customized using the `beeminder-value` property:

  - `prompt` -- Will prompt for a value to be submitted.
  - `time-today` -- Will send the time worked on the task today.
    Setting `beeminder-unit` to "minutes" will submit minutes worked, "hours"
    will submit hours.  Defaults to "hours" for backward compatibility.

Setting `beeminder-value` to anything else will submit that value.  If no
`beeminder-value` property is set, a value of "1" will be used.

Here's an example:

```org
* Beeminder.el Tasks
   :PROPERTIES:
   :beeminder: beeminder-el
   :END:
** TODO Write documentation
```

When "Write documentation" is changed to "DONE", a value of "1" will be submitted to the
"beeminder-el" goal with "Write documentation" as the comment.

A headlines properties will be updated with data from beeminder.com once it is
closed. For habit goals, all properties **except** `beeminder-value` are
updated. This can be changed via the `beeminder-excluded-habit-sync-properties`
variable.

For example, setting it to an empty list will synchronize `beeminder-value` with
the beeminder value.

```lisp
;; Synchronize ALL properties of the habit when closed.
(setq beeminder-excluded-habit-sync-properties '())

;; Prevent target value from being synced.
(setq beeminder-excluded-habit-sync-properties '("beeminder-target"))
```
