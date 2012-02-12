A collection of backends, formatters, and decorators for lager.  All configurations are done as property lists. 

Decorators
==========

Decorators are formatters that take another formatter as a parameter, then modify the output.  All require a parameter:
  
    {formatter, {Formatter, Config}}

This will take the output of lager_default_formatter and add ansi color codes to it for output.

lager\_gelf\_formatter
====================

Creates a json record of the log message, including all metadata, suitable for sending to graylog2.

Config
------

    {host, Name}  % name of the host to report.  default: "erlang"
    {facility, Name} % facility name used to report. default: "erlang"
    {short_message_size, N::integer()} % first N bytes of the message will be the short message. default: 80
    {utf8,boolean()} % whether to use utf8 or not.  default: true 

lager\_json\_formatter
====================
A basic JSON writer.  Creates individual objects for each message, converting the metadata properties to key: value pairs.

Config
------

    {utf8,boolean()} % whether to use utf8 or not.  default: true 

lager\_ansi\_color_decorator
==========================

Creates color for each severity that shows on the console.  Useful when dumping to the console, but will just junk up log files with ANSI color codes.

Config
------

    {formatter, {Formatter,Config}}  % The formatter and it's config that we will wrap.  Required.
    {algorithm, gzip | compress | zip } % The algorithm to use for compression.  Default: gzip
    {debug | info | notice | warning | error | critical | alert | emergency, color()} % override the color for a particular log level.  Optional.


lager\_zlib\_decorator
====================
  
Compressess the output in a variety of compression formats.  Note that the messages are compressed individually, so you can't write a compressed file this way due to the fact that gzip, compress, and zip do not concatenate in a straightforward manner.  This is largely intended to be used with the udp backend.

Config
------

    {formatter, {Formatter,Config}}  % The formatter and it's config that we will wrap.  Required.
    {algorithm, gzip | compress | zip } % The algorithm to use for compression.  Default: gzip

  
lager\_udp\_backend
=================

Sends the log message as a UDP packet to a host and port.

Config
------
    {host, any()}  % who to send the packets to.  required
    {port, integer()} % udp port to send to.  required

    {level, level()}  % threshold to report on this logger.  default: debug
    {formatter,atom()} % formatter.  default: lager_default_formatter),
    {format_config, any()} % configuraiton for the formatter.  default: []
    {inet_family, inet | inet6} % IPv4 or IPv6?  default: inet
    {name, any()}  % name of the backend (for trace support).  default: {Host,Port}
    
    
Example:  Graylog2 Config
=========================

Supporting Graylog2 is what started most of this.  In theory (haven't tested it yet), use this to send compressed gelf to a Graylog2 server
  
    {lager_udp_backend, [
        {formatter, lager_zlib_decorator},
        {format_config, [{algorithm, compress},
                         {formatter, {lager_gelf_formatter,[{host,"my server"},{facility:"erlang thingy"}]}}
                        ]},
        {host, "graylog2.mydomain.com"},
        {port, 12201},
        {level, debug},
        {name, graylog2}
    ]}

   
   