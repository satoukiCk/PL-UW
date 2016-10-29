def t i
    (0..i).each do |j|
        print " " * j
    (j..i).each {|k| print k; print ""}
    print "\n"
    end
end