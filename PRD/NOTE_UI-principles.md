# Shiny UI Principles

## App pattern

This applies to apps with a common workflow:

1. configure inputs (sidebar)
2. trigger an action
3. view results (main panel)
4. optionally export/share

Design for this flow explicitly.

---

## Core rules

- Organize UI around **user tasks**, not data structures
- Make **required vs optional inputs obvious**
- Keep **one primary action per screen**
- Prefer **clarity over flexibility**
- Reduce visual noise aggressively

---

## Layout

### Structure
- Left sidebar = setup (config + the one primary action)
- Main panel = results + interpretation
- Right pane = what the current settings produce (neutral, factual, live)
- Top of main panel = messages (warnings, errors, blockers, stale notices) - one place only

Do not overload the sidebar with explanation or multiple unrelated tasks. Do not put warnings
in the info pane or info in the messages area. EDARK's concrete page contract, widths and
per-page mapping: `BUILD_UI-redesign.md` Stage 4.

**Placement follows scope.** Ask what the control acts on, not where there is room:

| The control… | goes |
|---|---|
| changes the configuration | in the config pane |
| acts on the artefact already produced (save, copy, add to report, restyle) | in a small right-aligned toolbar directly above that artefact |
| states a fact about what the settings produce | in the info pane - which never holds a control |
| reports a problem | in the messages area - which never holds a fact or a control |

**Every pane must earn its width.** If a page leaves the info pane with nothing factual to
say, that is a sign the page is withholding something the user wants to know, not a reason to
drop the pane. Fill it.

**No surface that restates its own controls.** A "preview" that shows back the list the user
just assembled is dead weight. Delete it and give the space to the thing actually produced -
or, if that thing is a downloaded file, drop the centre column and let config + info span the
page.

---

### Sidebar

Keep it:
- flat (minimal borders)
- compact
- vertically scannable

Group controls into:

1. required inputs
2. optional inputs (visually secondary)
3. single primary action

Avoid:
- many nested boxes
- equal emphasis on all controls
- long unstructured stacks

---

### Sections

Use simple grouping:
- small uppercase section labels
- 1–3 related controls per section
- whitespace for separation

Do not wrap every section in a card.

---

## Actions

- One **primary button** (filled)
- Secondary actions = outline or subtle
- Avoid multiple competing primary buttons
- The primary action sits at the **bottom of the config pane**, full width - in *every* mode
  of the page. A page that puts Generate at the top in one mode and the bottom in another is
  telling the user the two modes are different things when they are not.

### One button scale

Two sizes, no exceptions:

| Scale | Class | Used for |
|---|---|---|
| config | `btn-<variant> w-100`, default height | any action inside a config pane |
| toolbar | `btn-sm btn-outline-<variant>` | any action on an already-produced artefact |

Dialog actions are default size with no width class. Write the class string **once**, in a
helper; a per-call-site class string is how an app ends up with `w-75` next to `w-100` and an
unstyled task button next to a primary one in the same pane. Components with their own
rendering (`bslib::input_task_button()`) go through the same helper rather than being an
exception at the call site.

---

## Dialogs

- Put the actions at the **top** of a dialog whose body is a list of unknown length, so they
  stay reachable; the list scrolls beneath them.
- Group them as they read: bulk edits on the left (Select All / Clear), commit and dismiss on
  the right (Done / Cancel).
- A dialog whose controls apply live needs no Cancel - only Done.

---

## Shared settings

A setting that applies to several screens - and to what those screens export - has:

- **one copy of the controls**, not one per screen
- **one set of stored values**, which every screen and every export path reads

When it is not a mode of the work but a way the work is displayed, put it behind a dialog
rather than in the config pane, where it would otherwise be duplicated per mode and sit
confusingly next to staged controls.

---

## Staged vs live

Do not stack controls that take effect on click and controls that take effect immediately in
the same pane with no distinction - the user cannot tell which is which, and learns to
distrust both. Either separate them visibly, or move the live ones out of the pane entirely.

---

## Inputs

- Use **consistent input types and styling**
- Do not mix visually different components without normalizing them
- Keep input height, spacing, and alignment uniform

---

## Task separation

If the app supports different actions (e.g., describe vs compare vs trend):

→ split into **tabs or modes**

Do not cram multiple workflows into one sidebar.

**Where the mode selector goes** depends on what the modes own:

- modes that share one result surface and differ only in settings → selector at the **top of
  the config pane**, with the settings it switches
- sub-steps that own their own settings *and* their own output → selector **across the top of
  the page**

Two placements, one visual family: same active colour, same weight, different size. Give each
level of navigation one look and keep it - an app that draws the same idea three ways makes
the user re-learn the layout on every page.

**Tabs inside the main panel are always card tabs** (`bslib::navset_card_tab`) - Prepare's
Columns / Transforms / Row Filters / Data Preview, Report's Sections / Preview, and every
Analyze result view alike. Pills and underline tabs belong above or beside the panes, never in
the result slot.

**Navigation labels are text only.** No icons or emoji in navbar tabs, pills, sub-steps or
card tabs - they add noise, compete with the label and drift out of step with each other. The
one exception is a *state* glyph put on by the server (the lock on a locked step, the check on
a done step), because it carries information the text does not.

---

## Defaults

- Preselect sensible values
- Let users get output quickly
- Hide advanced options behind collapsible sections

---

## Main panel

- Should never feel empty
- Show:
  - results
  - summaries
  - context
- Use empty states to guide users

---

## Visual hierarchy

Use:
- spacing
- typography

Avoid:
- excessive borders
- excessive icons
- decorative styling

---

## Common failure modes to avoid

- “form dump” sidebar
- too many buttons
- inconsistent input styling
- unclear primary action
- mixing multiple workflows in one panel
- everything looking equally important

---

## Guiding principle

The UI should feel like:
> a guided tool that helps the user complete one task at a time

Not:
> a collection of controls the user has to figure out