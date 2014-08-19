try:
	runghc scrapoo.hs | tee lasttry
full:
	runghc scrapoo.hs 999

.FORCE:

watch: .FORCE
	inotifywait . -rm | runghc watch.hs make scrapoo.hs sampletests\*

remowatch: .FORCE
	inotifywait . -rm | runghc watch.hs remotest scrapoo.hs sampletests\*

watchEx1: .FORCE
	inotifywait . -rm | runghc watch.hs 'runghc Example1.hs' Example1.hs
	
	#inotifywait . -rm | grep 'CLOSE_WRITE,CLOSE scrapoo.hs' | perl -pe 's/.*/make/' -- It blocks!

