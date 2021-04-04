# Why are there duplicate trajectories for some individuals? Some trajectories
# were recorded again after a problem occurred. Some trajectories were so long
# that the camera couldn't record them in their entirety, so the camera was
# restarted, sometimes multiple times. I think some specimens were mistakenly
# recorded more than once. Unfortunately, we didn't record when these things
# happened, so we need to try to work it out retrospectively.
#
# This script contains fragments of code to help identify when there are
# multiple trajectories per specimen, and how to handle it by working out which
# trajectory should be kept.

trjList <- LoadCachedTrajectories()



t <- table(trjList$metaInfo$imageableid)
t <- t[t > 1]
dupSpecimenIds <- names(t)


xx <- lapply(dupSpecimenIds, function(sid) do.call(rbind, lapply(which(trjList$metaInfo$imageableid == sid), function(i) data.frame(specimen = sid, video = trjList$metaInfo$id[i], duration = TrajDuration(trjList$trjs[[i]])))))
df <- do.call(rbind, xx)
#Don't do this again write.csv(df, "../dup-specimen-videos.csv", row.names = FALSE)

#View(trjList$metaInfo[trjList$metaInfo$imageableid == 762, ])

specimen <- 1172
specs <- trjList$metaInfo$imageableid == specimen
PlotTrjList(SubsetTrjInfo(trjList, specs), translate = F, main = sprintf("Specimen %d", specimen))
videos <- sort(trjList$metaInfo$id[specs])
cat(sprintf("Videos %s\n", JToSentence(videos)))

plot(trjList$trjs[[which(trjList$metaInfo$id == videos[1])]])
plot(trjList$trjs[[which(trjList$metaInfo$id == videos[2])]])
plot(trjList$trjs[[which(trjList$metaInfo$id == videos[3])]])
plot(trjList$trjs[[which(trjList$metaInfo$id == videos[4])]])
plot(trjList$trjs[[which(trjList$metaInfo$id == videos[5])]])
plot(trjList$trjs[[which(trjList$metaInfo$id == videos[6])]])
plot(trjList$trjs[[which(trjList$metaInfo$id == videos[7])]])




data <- "specimen	video	duration	Comment
762	3504	12.08	Low quality light duplicate
762	3505	13.36	ok
798	3510	17.6	ok
798	3511	10.26	Low quality light duplicate
862	3517	13.6	Low quality light duplicate
862	3571	12.32	ok
863	3518	5.88	ok
863	3654	6.14	Duplicate
947	3523	32.06	Low quality light duplicate
947	3522	9.16	ok
960	3633	35.26	ok
960	3633	23.12	Track split by video tracker, discard
960	3633	12.02	Track split by video tracker, discard
1003	4460	19.10833333	ok
1003	4459	3.258333333	Duplicate
1008	4454	4.25	ok
1008	4455	2.866666667	Duplicate
1065	4326	19.54583333	ok
1065	4474	16.29166667	Duplicate
1080	4330	17.91666667	ok
1080	4473	7.3125	Duplicate + uncooperative animal
1081	4328	18.7125	ok
1081	4472	15.25	Duplicate + uncooperative animal
1089	4471	19.25	ok
1089	4338	19.69166667	Duplicated video, now deleted
1096	4399	17.7875	5/7 ok
1096	4397	19.54166667	3/7 ok
1096	4400	19.3	6/7 ok
1096	4395	16.35416667	1/7 ok
1096	4401	14.72083333	7/7 ok
1096	4396	15.3375	2/7 ok
1096	4398	19.2	4/7 ok
1099	4405	16.45416667	2/2 ok
1099	4404	19.55833333	1/2 ok
1101	4408	13.99166667	2/2 ok
1101	4407	16.2	1/2 Low quality lighting
1102	4410	8.916666667	ok
1102	4409	10.375	Duplicate
1124	4616	3.504166667	Uncooperative
1124	4617	14.67083333	ok
1124	4614	12.02083333	Uncooperative
1124	4618	8.720833333	Uncooperative
1125	4621	19.69166667	1/2 ok
1125	4622	3.166666667	1/2 very short
1130	4627	8.833333333	2/2 ok
1130	4625	17.675	1/2 ok duplicate
1130	4623	18.62083333	1/2 ok
1130	4624	8.766666667	2/2 ok
1134	4629	14.64583333	2/2 ok
1134	4631	6.758333333	1/2 ok
1134	4630	16.67083333	Duplicate
1137	4634	17.91666667	1/2 ok
1137	4635	13.325	2/2 ok
1138	4638	19.69166667	2/3 ok
1138	4636	10.17916667	3/3 ok
1138	4637	18.2375	1/3 ok
1139	4639	8.55	Duplicate
1139	4640	8.275	ok
1140	4642	18.62083333	1/3 ok
1140	4643	19.69166667	2/3 ok
1140	4641	13.21666667	3/3 ok
1141	4649	15.3125	Low quality light duplicate
1141	4644	19.69166667	Low quality light duplicate
1141	4647	10.9375	Low quality light duplicate
1141	4645	9.1875	Low quality light duplicate
1141	4648	19.69166667	Low quality light duplicate
1141	4650	19.475	ok
1141	4646	2.458333333	Low quality light duplicate
1149	4654	17.3625	2/2 ok
1149	4653	4.3	1/2 ok
1150	4656	16.27083333	1/3 ok
1150	4655	19.69166667	3/3 ok
1150	4657	19.69166667	2/3 ok
1151	4659	19.69166667	2/2 ok
1151	4658	17.67916667	1/2 ok
1152	4661	17.99583333	1/3 ok
1152	4660	11.95833333	3/3 short
1152	4662	19.69166667	2/3 ok
1152	4662	19.69166667	duplicate uncooperative
1153	4664	17.0875	2/2 ok
1153	4663	14.88333333	1/2 ok
1158	4667	19.2375	1/2 ok
1158	4668	15.1875	2/2 ok
1159	4669	19.05833333	2/2 ok
1159	4672	18.95833333	1/2 ok
1159	4671	15.8125	2/2 duplicate
1159	4670	9.708333333	2/2 duplicate
1160	4673	19.04583333	1/2 ok
1160	4674	11.3	2/2 ok
1164	4681	7.058333333	Uncooperative
1164	4679	5.270833333	ok
1166	4685	10.625	4/4 ok
1166	4683	13.97083333	2/4 ok
1166	4684	19.5	3/4 ok
1166	4682	19.275	1/4 ok
1169	4693	18.57916667	1/3 ok
1169	4694	18.90833333	2/3 ok
1169	4692	11.05	3/3 ok
1171	4696	7.341666667	ok
1171	4695	4.454166667	Uncooperative
1172	4698	5.625	1/2 ok
1172	4699	3.025	Uncooperative
1172	4697	2.770833333	2/2 ok
1172	4700	10.30833333	Uncooperative
1173	4701	8.7875	1/2 ok
1173	4702	5.8375	2/2 ok
1174	4704	19.68333333	2/2 ok
1174	4703	19.6875	1/2 ok
1175	4706	3.9125	2/2 ok
1175	4705	18.85	1/2 ok
1180	4716	12.36666667	2/2 ok
1180	4714	18.80416667	2/2 ok
1180	4711	18.95416667	1/2 ok
1180	4712	7.608333333	2/2 ok
1180	4713	19.29583333	1/2 ok duplicate
1180	4715	19.1375	1/2 ok duplicate
1182	4718	8.4375	2/2 ok
1182	4717	19.13333333	1/2 ok
1184	4720	19.0125	ok
1184	4719	16.1875	Uncooperative
1185	4722	6.675	2/2 ok
1185	4721	18.67916667	1/2 ok
1188	4723	11.8875	1/2 ok
1188	4724	14.89583333	2/2 ok
1190	4726	8.1625	2/2 ok
1190	4725	19.69166667	1/2 ok
1197	4732	19.32083333	1/3 ok
1197	4731	3.404166667	3/3 ok
1197	4733	13.95416667	2/3 ok
"
assessments <- read.table(text = data, sep = "\t", header = TRUE)
got <- grep("^ok$|^1/.*ok$", assessments$Comment)
# Specimen 1102 first trajectory has crappy lighting, use trajectory 2
got <- c(got, match(4408, assessments$video))
dput(got)

gotSpecimens <- assessments[got,]
# Check
setdiff(unique(assessments$specimen), gotSpecimens$specimen)

# Rows to chuck
dput(apply(assessments[-got,c("specimen", "video")], MARGIN = 1, paste, collapse = "-"))
print(assessments[-got, c("specimen", "video", "Comment")], row.names = F)
