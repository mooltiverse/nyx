# Changelog


{{#releases}}
## {{name}} ({{date}})

{{#sections}}
### {{name}}

{{#commits}}
* [{{#short5}}{{SHA}}{{/short5}}] {{message.shortMessage}} ({{authorAction.identity.name}})

{{/commits}}
{{^commits}}
No changes.
{{/commits}}
{{/sections}}
{{^sections}}
No changes.
{{/sections}}
{{/releases}}
{{^releases}}
No releases.
{{/releases}}
