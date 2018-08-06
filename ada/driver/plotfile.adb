with ada.strings.fixed,text_io,framework,digsimio;
use  ada.strings.fixed,text_io,framework,digsimio;
procedure plotfile(basename : string) is
   plot : file_type;
begin
   if nplots > 0 then
      create(plot,out_file,basename & ".plt");
      put_line(plot,"set terminal pdf font ""Arial Italic, 12""");
      put_line(plot,"set output """ & basename & ".pdf""");
      put_line(plot,"# remove border on top and right and set color to gray");
      put_line(plot,"set style line 11 linecolor rgb 'black' linetype 1");
      put_line(plot,"set border 3 back linestyle 11");
      put_line(plot,"set tics nomirror");
      put_line(plot,"set style line 12 linecolor rgb 'black' lt 0 linewidth 1");
      put_line(plot,"set grid back linestyle 12");
      put_line(plot,"set nokey");
      put_line(plot,"set zeroaxis linetype 3 linewidth 2.5");
      put_line(plot,"set style line 1 linetype 1 linewidth 1 lc 'red'");
      for i in 1 .. nplots loop
         declare
            title : string := names'image(print_list(plotx_list(i))) &
                      " vs. " & names'image(print_list(ploty_list(i)));
            xlabel : string := cell(print_list(plotx_list(i))).desc.all;
            ylabel : string := cell(print_list(ploty_list(i))).desc.all;
            xindex : string := trim(integer'image(plotx_list(i)),ada.strings.left);
            yindex : string := trim(integer'image(ploty_list(i)),ada.strings.left);
         begin
            put_line(plot,"set title """   & title    & """");
            put_line(plot,"set xlabel """  & xlabel   & """");
            put_line(plot,"set ylabel """ & ylabel   & """  rotate by 90");
            put_line(plot,"plot  """       & basename & ".dat""" & " using " & xindex & ":" & yindex & " with lines ls 1");
         end;
      end loop;
      close(plot);
   end if;
end plotfile;
