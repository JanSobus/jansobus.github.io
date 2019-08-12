function knight()
%Q1- number of moves for a=1, b=2, n=5
q1=sol(1,2,5)
%Q2 - number of "impossible" knights for n=5
count=0;
for i=1:4
    for j=1:i
        m=sol(j,i,5);
        if (m == 0)
            count=count+1;
        end
    end
end
q2=count
%Q3 - total moves for a<=b, n=5
count=0;
for i=1:4
    for j=1:i
        m=sol(j,i,5);
        count=count+m;
    end
end
q3=count
%Q4 - number of moves for a=4, b=7, n=25
q4=sol(4,7,25)
%Q5 - number of "impossible" knights for n=25
count=0;
for i=1:24
    for j=1:i
        m=sol(j,i,25);
        if (m == 0)
            count=count+1;
        end
    end
end
q5=count
%Q6 - total moves for a<=b, n=25
count= 0;
for i=1:24
    for j=1:i
        m=sol(j,i,25);
        count=count+m;
    end
end
q6=count

%Q7 - number of moves for a=13, b=23, n=1000
q7=sol(13,23,1000)
%Q8 - number of moves for a=73, b=101, n=10000
q8=sol(73,101,10000)
end


%function that takes a,b (a<=b) and boardsize n and returns minimum number
%of moves or 0 if reaching (n-1,n-1) is impossible for given a,b

function nmoves=sol(a,b,boardsize)
n=boardsize-1;
if (a==b)
    if (mod(n,a) == 0)
        nmoves= n/a;
        return
    end
else
    if ( (a+b) > n)
        if (b == n)
            if (mod(n,a) == 0)
                if (mod(n/a,2) == 0)
                    nmoves=2*n/a;
                    return
                else
                    nmoves=n/a;
                    return
                end
            end
        else
            if (mod(n,2*a) == 0)
                nmoves=2*n/a;
                return
            end
        end
    else
        d1=(a+b)/2;
        d2=(b-a)/2;
        
        dim=2*n;
        
        lin_combs=d1*ones(2*dim+1,1)*[-dim:dim]+d2*[-dim:dim]'*ones(1,2*dim+1);
        [nd2,nd1] = find(ismember(lin_combs,n));
        solutions = sortrows([nd1-dim-1,nd2-dim-1,abs(nd2-(dim+1))+abs(nd1-(dim+1))],3);
        for i=1:length(solutions(:,1))
            if (solutions(i,1) > 0)
                off_diag=d2*ones(abs(solutions(i,2))+1,1)*[-abs(solutions(i,1)):2:abs(solutions(i,1))]+d1*[-abs(solutions(i,2)):2:abs(solutions(i,2))]'*ones(1,abs(solutions(i,1))+1);
                if (max(max(ismember(off_diag,0))))
                    nmoves = solutions(i,3);
                    return
                end
            end
        end
    end
end
nmoves = 0;
end
