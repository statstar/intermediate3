# 로지스틱 회귀모형은 크게 1. 이분형 로지스틱 회귀모형, 
# 2. 다항 로지스틱 회귀모형, 3 순서형 로지스틱 회귀모형으로 구분된다.

# 기본형은 이분형 로지스틱 회귀모형이다. glm 함수 (family=binomial(link="logit"))
# 다항 로지스틱은 종속변수의 그룹에 따라 절변과 회귀계수가 모두 달라지는 모형식으로 
# multinom 함수를 이용하면 된다.

# 만약 종속변수가 명목형이 아닌 순서형인 경우 순서형 로지스틱 회귀모형을 세울 수 있고,
# 다항 로지스틱 회귀모형과 차이점은 회귀계수는 동일하지만 자료에 따른 절편만 다르다는 것이다. 
# p.160~p.183

if(!require(foreign)) install.packages("foreign"); library(foreign)

dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
head(dat)
table(dat$apply) # 빈도표 작성 
 # 대학원 진학 가능성 1. unlikely, 2. somewhat likely, 3. very likely
table(dat$pared) # 빈도표 작성
 # 한부모여부 0. 부모님이 모두 계심, 1. 한부모만 계심
table(dat$public)
 # 졸업학교가 0. 공립, 1. 사립
boxplot(dat$gpa)
 # 학점

# 연구모형 : 한부모여부, 졸업학교, 학점에 따라 대학원 진학 가능성을 분류하는 모형
# 회귀모형에 앞서 교차분석을 통해 대학원 진학 가능성과 변수 간 연관성이 있는지 살펴보자. (교차분석, 일원배치분산분석)

xtabs(~apply+pared, data=dat)
if(!require(gmodels)) install.packages("gmodels"); library(gmodels) #교차분석을 위한 팩키지 설치
CrossTable(dat$apply, dat$pared, chisq=TRUE) #유의미한 차이

xtabs(~apply+public, data=dat)
CrossTable(dat$apply, dat$public, chisq=TRUE) #유의미하지 않은 차이

boxplot(gpa~apply, data=dat)
out<-aov(gpa~apply, data=dat)
summary(out) # 유의확률이 낮으므로 대학원 진학 가능성에 따라 학점에 차이가 존재
TukeyHSD(out) # Tukey의 사후분석을 통해 어느 집단에서 차이를 보이는지 확인

# 순서형 척도를 종속변수로하는 로지스틱 회귀모형의 가장 중요한 가정은 회귀계수가 같다는 것이다.
# 만약 y가 1,2,3인 경우 1 vs 2,3의 OR에 미치는 영향과  1,2 vs 3의 OR에 미치는 영향이 같다고 가정한다.
# 즉, 회귀계수는 같지만 절편만 다른 모형이다. 

library(MASS)
# help(polr)
ologit <-  polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE) # MASS 패키지의 polr함수를 이용해서 분석
summary(ologit)

# 이제 추정된 회귀계수에 exponential을 취해 Odds ratio로 결과를 해석하면 된다.
exp(coef(ologit))

#순서척도 로지스틱 회귀분석에서 OR은 proportional OR이라고도 부른다. 
# 다른 변수가 고정되어있을 때, pared=0일 때에 비하여 pared=1일 때, “very likely” vs “somewhat likely” or “unlikely”의 OR이 2.851이다. 
# 다른 변수가 고정되어있을 때, pared=0일 때에 비하여 pared=1일 때, “very likely” or “somewhat likely” vs “unlikely”의 OR이 2.851이다. 
# 이는 즉, pared=1일 때, 대학원 진학(apply)을 할 가능성이 높다는 것을 뜻한다.
# 같은 binary 변수인 public도 위와 마찬가지로 해석한다.
# 다른 변수가 고정되어있을 때, gpa가 1 증가할 때, “very likely” vs “somewhat likely” or “unlikely” Odds가 1.85배가 된다. 
# 다른 변수가 고정되어있을 때, gpa가 1 증가할 때, “very likely” or “somewhat likely” vs “unlikely” Odds가 1.85배가 된다.

#출처: https://3months.tistory.com/245 [Deep Play]
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

