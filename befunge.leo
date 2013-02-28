<?xml version="1.0" encoding="utf-8"?>
<!-- Created by Leo (http://webpages.charter.net/edreamleo/front.html) -->
<?xml-stylesheet ekr_test?>
<leo_file xmlns:leo="http://www.leo-editor.org/2011/leo" >
<leo_header file_format="2" tnodes="0" max_tnode_index="0" clone_windows="0"/>
<globals body_outline_ratio="0.5" body_secondary_ratio="0.5">
	<global_window_position top="50" left="50" height="500" width="700"/>
	<global_log_window_position top="0" left="0" height="0" width="0"/>
</globals>
<preferences/>
<find_panel_settings/>
<vnodes>
<v t="kajsa.20130215175302.1357" a="E"><vh>Problem</vh>
<v t="kajsa.20130215180649.1362"><vh>Parsing a simple program</vh></v>
</v>
<v t="kajsa.20130215185232.1373"><vh>TODO</vh></v>
<v t="kajsa.20130215183429.1368" a="E"><vh>Program</vh>
<v t="kajsa.20130215183429.1369"><vh>Interpreter</vh></v>
<v t="kajsa.20130215183429.1371"><vh>Commands</vh></v>
<v t="kajsa.20130215183429.1370" a="E"><vh>Files</vh>
<v t="kajsa.20130215180649.1361" a="E"><vh>@file BefungeParser.hs</vh></v>
</v>
</v>
</vnodes>
<tnodes>
<t tx="kajsa.20130215175302.1357">We want to parse a Befunge program.

</t>
<t tx="kajsa.20130215180649.1362">When inputing a program containing 0 " &gt; : # , _ @ we want the expected output.

E.g input

    0"!dlroW ,olleH"&gt;:#,_@

and output

    Hello, World!</t>
<t tx="kajsa.20130215183429.1368"></t>
<t tx="kajsa.20130215183429.1369">runBefungeProgram :: String -&gt; String
runBefungeProgram program = interpretProgram program []

interpretProgram :: String -&gt; String -&gt; String
interpretProgram ('"':xs)       stack     = interpretString xs stack
interpretProgram (',':xs)       (s:stack) = s:(interpretProgram xs stack)
interpretProgram ('@':xs)       stack     = ""
interpretProgram   (x:xs)       stack     = interpretProgram xs stack

interpretString :: String -&gt; String -&gt; String
interpretString ('"':xs) stack = interpretProgram xs stack
interpretString (c:xs) stack = interpretString xs (c:stack)</t>
<t tx="kajsa.20130215183429.1370"></t>
<t tx="kajsa.20130215183429.1371"></t>
<t tx="kajsa.20130215185232.1373">* investigate whether external editors are usable for file nodes</t>
</tnodes>
</leo_file>
