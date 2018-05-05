; ModuleID = 'PixCzar'

%frame = type { %placement_node* }
%placement_node = type { %placement_node*, %placement* }
%placement = type { %pix*, i32, i32, i32, i32 }
%pix = type { i32, i8*, i32, i32, i32* }

@tmp = private unnamed_addr constant [17 x i8] c"images/shrek.png\00"
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @render(i32, %frame**, i32, i32, i32, ...)

define void @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 mul (i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32), i32 3))
  %array_gen = bitcast i8* %malloccall to i32*
  %array_assign = getelementptr i32* %array_gen, i32 0
  store i32 200, i32* %array_assign
  %array_assign1 = getelementptr i32* %array_gen, i32 1
  store i32 200, i32* %array_assign1
  %array_assign2 = getelementptr i32* %array_gen, i32 2
  store i32 200, i32* %array_assign2
  %arr = alloca i32*
  store i32* %array_gen, i32** %arr
  %malloccall3 = tail call i8* @malloc(i32 ptrtoint (%pix* getelementptr (%pix* null, i32 1) to i32))
  %malloc = bitcast i8* %malloccall3 to %pix*
  %struct_build = getelementptr inbounds %pix* %malloc, i32 0, i32 0
  store i32 0, i32* %struct_build
  %struct_build4 = getelementptr inbounds %pix* %malloc, i32 0, i32 1
  store i8* null, i8** %struct_build4
  %struct_build5 = getelementptr inbounds %pix* %malloc, i32 0, i32 2
  store i32 0, i32* %struct_build5
  %struct_build6 = getelementptr inbounds %pix* %malloc, i32 0, i32 3
  store i32 0, i32* %struct_build6
  %struct_build7 = getelementptr inbounds %pix* %malloc, i32 0, i32 4
  store i32* null, i32** %struct_build7
  %test = alloca %pix*
  store %pix* %malloc, %pix** %test
  %test8 = load %pix** %test
  %arr9 = load i32** %arr
  %struct_build10 = getelementptr inbounds %pix* %test8, i32 0, i32 0
  store i32 2, i32* %struct_build10
  %struct_build11 = getelementptr inbounds %pix* %test8, i32 0, i32 1
  store i8* null, i8** %struct_build11
  %struct_build12 = getelementptr inbounds %pix* %test8, i32 0, i32 2
  store i32 0, i32* %struct_build12
  %struct_build13 = getelementptr inbounds %pix* %test8, i32 0, i32 3
  store i32 200, i32* %struct_build13
  %struct_build14 = getelementptr inbounds %pix* %test8, i32 0, i32 4
  store i32* %arr9, i32** %struct_build14
  %malloccall15 = tail call i8* @malloc(i32 ptrtoint (%pix* getelementptr (%pix* null, i32 1) to i32))
  %malloc16 = bitcast i8* %malloccall15 to %pix*
  %struct_build17 = getelementptr inbounds %pix* %malloc16, i32 0, i32 0
  store i32 0, i32* %struct_build17
  %struct_build18 = getelementptr inbounds %pix* %malloc16, i32 0, i32 1
  store i8* null, i8** %struct_build18
  %struct_build19 = getelementptr inbounds %pix* %malloc16, i32 0, i32 2
  store i32 0, i32* %struct_build19
  %struct_build20 = getelementptr inbounds %pix* %malloc16, i32 0, i32 3
  store i32 0, i32* %struct_build20
  %struct_build21 = getelementptr inbounds %pix* %malloc16, i32 0, i32 4
  store i32* null, i32** %struct_build21
  %rect = alloca %pix*
  store %pix* %malloc16, %pix** %rect
  %rect22 = load %pix** %rect
  %arr23 = load i32** %arr
  %struct_build24 = getelementptr inbounds %pix* %rect22, i32 0, i32 0
  store i32 1, i32* %struct_build24
  %struct_build25 = getelementptr inbounds %pix* %rect22, i32 0, i32 1
  store i8* null, i8** %struct_build25
  %struct_build26 = getelementptr inbounds %pix* %rect22, i32 0, i32 2
  store i32 100, i32* %struct_build26
  %struct_build27 = getelementptr inbounds %pix* %rect22, i32 0, i32 3
  store i32 200, i32* %struct_build27
  %struct_build28 = getelementptr inbounds %pix* %rect22, i32 0, i32 4
  store i32* %arr23, i32** %struct_build28
  %malloccall29 = tail call i8* @malloc(i32 ptrtoint (%pix* getelementptr (%pix* null, i32 1) to i32))
  %malloc30 = bitcast i8* %malloccall29 to %pix*
  %struct_build31 = getelementptr inbounds %pix* %malloc30, i32 0, i32 0
  store i32 0, i32* %struct_build31
  %struct_build32 = getelementptr inbounds %pix* %malloc30, i32 0, i32 1
  store i8* null, i8** %struct_build32
  %struct_build33 = getelementptr inbounds %pix* %malloc30, i32 0, i32 2
  store i32 0, i32* %struct_build33
  %struct_build34 = getelementptr inbounds %pix* %malloc30, i32 0, i32 3
  store i32 0, i32* %struct_build34
  %struct_build35 = getelementptr inbounds %pix* %malloc30, i32 0, i32 4
  store i32* null, i32** %struct_build35
  %el = alloca %pix*
  store %pix* %malloc30, %pix** %el
  %el36 = load %pix** %el
  %arr37 = load i32** %arr
  %struct_build38 = getelementptr inbounds %pix* %el36, i32 0, i32 0
  store i32 3, i32* %struct_build38
  %struct_build39 = getelementptr inbounds %pix* %el36, i32 0, i32 1
  store i8* null, i8** %struct_build39
  %struct_build40 = getelementptr inbounds %pix* %el36, i32 0, i32 2
  store i32 100, i32* %struct_build40
  %struct_build41 = getelementptr inbounds %pix* %el36, i32 0, i32 3
  store i32 200, i32* %struct_build41
  %struct_build42 = getelementptr inbounds %pix* %el36, i32 0, i32 4
  store i32* %arr37, i32** %struct_build42
  %malloccall43 = tail call i8* @malloc(i32 ptrtoint (%pix* getelementptr (%pix* null, i32 1) to i32))
  %malloc44 = bitcast i8* %malloccall43 to %pix*
  %struct_build45 = getelementptr inbounds %pix* %malloc44, i32 0, i32 0
  store i32 0, i32* %struct_build45
  %struct_build46 = getelementptr inbounds %pix* %malloc44, i32 0, i32 1
  store i8* null, i8** %struct_build46
  %struct_build47 = getelementptr inbounds %pix* %malloc44, i32 0, i32 2
  store i32 0, i32* %struct_build47
  %struct_build48 = getelementptr inbounds %pix* %malloc44, i32 0, i32 3
  store i32 0, i32* %struct_build48
  %struct_build49 = getelementptr inbounds %pix* %malloc44, i32 0, i32 4
  store i32* null, i32** %struct_build49
  %text = alloca %pix*
  store %pix* %malloc44, %pix** %text
  %text50 = load %pix** %text
  %struct_build51 = getelementptr inbounds %pix* %text50, i32 0, i32 0
  store i32 4, i32* %struct_build51
  %struct_build52 = getelementptr inbounds %pix* %text50, i32 0, i32 1
  store i8* getelementptr inbounds ([17 x i8]* @tmp, i32 0, i32 0), i8** %struct_build52
  %struct_build53 = getelementptr inbounds %pix* %text50, i32 0, i32 2
  store i32 200, i32* %struct_build53
  %struct_build54 = getelementptr inbounds %pix* %text50, i32 0, i32 3
  store i32 200, i32* %struct_build54
  %el55 = load %pix** %el
  %malloccall56 = tail call i8* @malloc(i32 ptrtoint (%placement* getelementptr (%placement* null, i32 1) to i32))
  %malloc57 = bitcast i8* %malloccall56 to %placement*
  %struct_build58 = getelementptr inbounds %placement* %malloc57, i32 0, i32 0
  store %pix* %el55, %pix** %struct_build58
  %struct_build59 = getelementptr inbounds %placement* %malloc57, i32 0, i32 1
  store i32 100, i32* %struct_build59
  %struct_build60 = getelementptr inbounds %placement* %malloc57, i32 0, i32 2
  store i32 300, i32* %struct_build60
  %struct_build61 = getelementptr inbounds %placement* %malloc57, i32 0, i32 3
  store i32 3, i32* %struct_build61
  %struct_build62 = getelementptr inbounds %placement* %malloc57, i32 0, i32 4
  store i32 5, i32* %struct_build62
  %placed = alloca %placement*
  store %placement* %malloc57, %placement** %placed
  %text63 = load %pix** %text
  %malloccall64 = tail call i8* @malloc(i32 ptrtoint (%placement* getelementptr (%placement* null, i32 1) to i32))
  %malloc65 = bitcast i8* %malloccall64 to %placement*
  %struct_build66 = getelementptr inbounds %placement* %malloc65, i32 0, i32 0
  store %pix* %text63, %pix** %struct_build66
  %struct_build67 = getelementptr inbounds %placement* %malloc65, i32 0, i32 1
  store i32 200, i32* %struct_build67
  %struct_build68 = getelementptr inbounds %placement* %malloc65, i32 0, i32 2
  store i32 300, i32* %struct_build68
  %struct_build69 = getelementptr inbounds %placement* %malloc65, i32 0, i32 3
  store i32 3, i32* %struct_build69
  %struct_build70 = getelementptr inbounds %placement* %malloc65, i32 0, i32 4
  store i32 5, i32* %struct_build70
  %placed1 = alloca %placement*
  store %placement* %malloc65, %placement** %placed1
  %test71 = load %pix** %test
  %malloccall72 = tail call i8* @malloc(i32 ptrtoint (%placement* getelementptr (%placement* null, i32 1) to i32))
  %malloc73 = bitcast i8* %malloccall72 to %placement*
  %struct_build74 = getelementptr inbounds %placement* %malloc73, i32 0, i32 0
  store %pix* %test71, %pix** %struct_build74
  %struct_build75 = getelementptr inbounds %placement* %malloc73, i32 0, i32 1
  store i32 300, i32* %struct_build75
  %struct_build76 = getelementptr inbounds %placement* %malloc73, i32 0, i32 2
  store i32 300, i32* %struct_build76
  %struct_build77 = getelementptr inbounds %placement* %malloc73, i32 0, i32 3
  store i32 3, i32* %struct_build77
  %struct_build78 = getelementptr inbounds %placement* %malloc73, i32 0, i32 4
  store i32 5, i32* %struct_build78
  %placed2 = alloca %placement*
  store %placement* %malloc73, %placement** %placed2
  %rect79 = load %pix** %rect
  %malloccall80 = tail call i8* @malloc(i32 ptrtoint (%placement* getelementptr (%placement* null, i32 1) to i32))
  %malloc81 = bitcast i8* %malloccall80 to %placement*
  %struct_build82 = getelementptr inbounds %placement* %malloc81, i32 0, i32 0
  store %pix* %rect79, %pix** %struct_build82
  %struct_build83 = getelementptr inbounds %placement* %malloc81, i32 0, i32 1
  store i32 400, i32* %struct_build83
  %struct_build84 = getelementptr inbounds %placement* %malloc81, i32 0, i32 2
  store i32 300, i32* %struct_build84
  %struct_build85 = getelementptr inbounds %placement* %malloc81, i32 0, i32 3
  store i32 3, i32* %struct_build85
  %struct_build86 = getelementptr inbounds %placement* %malloc81, i32 0, i32 4
  store i32 5, i32* %struct_build86
  %placed3 = alloca %placement*
  store %placement* %malloc81, %placement** %placed3
  %test87 = load %pix** %test
  %malloccall88 = tail call i8* @malloc(i32 ptrtoint (%placement* getelementptr (%placement* null, i32 1) to i32))
  %malloc89 = bitcast i8* %malloccall88 to %placement*
  %struct_build90 = getelementptr inbounds %placement* %malloc89, i32 0, i32 0
  store %pix* %test87, %pix** %struct_build90
  %struct_build91 = getelementptr inbounds %placement* %malloc89, i32 0, i32 1
  store i32 500, i32* %struct_build91
  %struct_build92 = getelementptr inbounds %placement* %malloc89, i32 0, i32 2
  store i32 300, i32* %struct_build92
  %struct_build93 = getelementptr inbounds %placement* %malloc89, i32 0, i32 3
  store i32 3, i32* %struct_build93
  %struct_build94 = getelementptr inbounds %placement* %malloc89, i32 0, i32 4
  store i32 5, i32* %struct_build94
  %placed4 = alloca %placement*
  store %placement* %malloc89, %placement** %placed4
  %test95 = load %pix** %test
  %malloccall96 = tail call i8* @malloc(i32 ptrtoint (%placement* getelementptr (%placement* null, i32 1) to i32))
  %malloc97 = bitcast i8* %malloccall96 to %placement*
  %struct_build98 = getelementptr inbounds %placement* %malloc97, i32 0, i32 0
  store %pix* %test95, %pix** %struct_build98
  %struct_build99 = getelementptr inbounds %placement* %malloc97, i32 0, i32 1
  store i32 600, i32* %struct_build99
  %struct_build100 = getelementptr inbounds %placement* %malloc97, i32 0, i32 2
  store i32 300, i32* %struct_build100
  %struct_build101 = getelementptr inbounds %placement* %malloc97, i32 0, i32 3
  store i32 3, i32* %struct_build101
  %struct_build102 = getelementptr inbounds %placement* %malloc97, i32 0, i32 4
  store i32 5, i32* %struct_build102
  %placed5 = alloca %placement*
  store %placement* %malloc97, %placement** %placed5
  %malloccall103 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32), i32 6))
  %array_gen104 = bitcast i8* %malloccall103 to %frame**
  %frames = alloca %frame**
  store %frame** %array_gen104, %frame*** %frames
  %malloccall105 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc106 = bitcast i8* %malloccall105 to %placement_node*
  %struct_build107 = getelementptr inbounds %placement_node* %malloc106, i32 0, i32 0
  store %placement_node* null, %placement_node** %struct_build107
  %struct_build108 = getelementptr inbounds %placement_node* %malloc106, i32 0, i32 1
  store %placement* null, %placement** %struct_build108
  %malloccall109 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %malloc110 = bitcast i8* %malloccall109 to %frame*
  %struct_build111 = getelementptr inbounds %frame* %malloc110, i32 0, i32 0
  store %placement_node* %malloc106, %placement_node** %struct_build111
  %frames112 = load %frame*** %frames
  %arr_access = getelementptr %frame** %frames112, i32 0
  store %frame* %malloc110, %frame** %arr_access
  %malloccall113 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc114 = bitcast i8* %malloccall113 to %placement_node*
  %struct_build115 = getelementptr inbounds %placement_node* %malloc114, i32 0, i32 0
  store %placement_node* null, %placement_node** %struct_build115
  %struct_build116 = getelementptr inbounds %placement_node* %malloc114, i32 0, i32 1
  store %placement* null, %placement** %struct_build116
  %malloccall117 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %malloc118 = bitcast i8* %malloccall117 to %frame*
  %struct_build119 = getelementptr inbounds %frame* %malloc118, i32 0, i32 0
  store %placement_node* %malloc114, %placement_node** %struct_build119
  %frames120 = load %frame*** %frames
  %arr_access121 = getelementptr %frame** %frames120, i32 1
  store %frame* %malloc118, %frame** %arr_access121
  %malloccall122 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc123 = bitcast i8* %malloccall122 to %placement_node*
  %struct_build124 = getelementptr inbounds %placement_node* %malloc123, i32 0, i32 0
  store %placement_node* null, %placement_node** %struct_build124
  %struct_build125 = getelementptr inbounds %placement_node* %malloc123, i32 0, i32 1
  store %placement* null, %placement** %struct_build125
  %malloccall126 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %malloc127 = bitcast i8* %malloccall126 to %frame*
  %struct_build128 = getelementptr inbounds %frame* %malloc127, i32 0, i32 0
  store %placement_node* %malloc123, %placement_node** %struct_build128
  %frames129 = load %frame*** %frames
  %arr_access130 = getelementptr %frame** %frames129, i32 2
  store %frame* %malloc127, %frame** %arr_access130
  %malloccall131 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc132 = bitcast i8* %malloccall131 to %placement_node*
  %struct_build133 = getelementptr inbounds %placement_node* %malloc132, i32 0, i32 0
  store %placement_node* null, %placement_node** %struct_build133
  %struct_build134 = getelementptr inbounds %placement_node* %malloc132, i32 0, i32 1
  store %placement* null, %placement** %struct_build134
  %malloccall135 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %malloc136 = bitcast i8* %malloccall135 to %frame*
  %struct_build137 = getelementptr inbounds %frame* %malloc136, i32 0, i32 0
  store %placement_node* %malloc132, %placement_node** %struct_build137
  %frames138 = load %frame*** %frames
  %arr_access139 = getelementptr %frame** %frames138, i32 3
  store %frame* %malloc136, %frame** %arr_access139
  %malloccall140 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc141 = bitcast i8* %malloccall140 to %placement_node*
  %struct_build142 = getelementptr inbounds %placement_node* %malloc141, i32 0, i32 0
  store %placement_node* null, %placement_node** %struct_build142
  %struct_build143 = getelementptr inbounds %placement_node* %malloc141, i32 0, i32 1
  store %placement* null, %placement** %struct_build143
  %malloccall144 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %malloc145 = bitcast i8* %malloccall144 to %frame*
  %struct_build146 = getelementptr inbounds %frame* %malloc145, i32 0, i32 0
  store %placement_node* %malloc141, %placement_node** %struct_build146
  %frames147 = load %frame*** %frames
  %arr_access148 = getelementptr %frame** %frames147, i32 4
  store %frame* %malloc145, %frame** %arr_access148
  %malloccall149 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc150 = bitcast i8* %malloccall149 to %placement_node*
  %struct_build151 = getelementptr inbounds %placement_node* %malloc150, i32 0, i32 0
  store %placement_node* null, %placement_node** %struct_build151
  %struct_build152 = getelementptr inbounds %placement_node* %malloc150, i32 0, i32 1
  store %placement* null, %placement** %struct_build152
  %malloccall153 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %malloc154 = bitcast i8* %malloccall153 to %frame*
  %struct_build155 = getelementptr inbounds %frame* %malloc154, i32 0, i32 0
  store %placement_node* %malloc150, %placement_node** %struct_build155
  %frames156 = load %frame*** %frames
  %arr_access157 = getelementptr %frame** %frames156, i32 5
  store %frame* %malloc154, %frame** %arr_access157
  %frames158 = load %frame*** %frames
  %arr_access159 = getelementptr %frame** %frames158, i32 0
  %arr_access_val = load %frame** %arr_access159
  %placed160 = load %placement** %placed
  %add_plcmt = getelementptr inbounds %frame* %arr_access_val, i32 0, i32 0
  %node = load %placement_node** %add_plcmt
  %malloccall161 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc162 = bitcast i8* %malloccall161 to %placement_node*
  %struct_build163 = getelementptr inbounds %placement_node* %malloc162, i32 0, i32 0
  store %placement_node* %node, %placement_node** %struct_build163
  %struct_build164 = getelementptr inbounds %placement_node* %malloc162, i32 0, i32 1
  store %placement* %placed160, %placement** %struct_build164
  store %placement_node* %malloc162, %placement_node** %add_plcmt
  %frames165 = load %frame*** %frames
  %arr_access166 = getelementptr %frame** %frames165, i32 1
  %arr_access_val167 = load %frame** %arr_access166
  %placed1168 = load %placement** %placed1
  %add_plcmt169 = getelementptr inbounds %frame* %arr_access_val167, i32 0, i32 0
  %node170 = load %placement_node** %add_plcmt169
  %malloccall171 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc172 = bitcast i8* %malloccall171 to %placement_node*
  %struct_build173 = getelementptr inbounds %placement_node* %malloc172, i32 0, i32 0
  store %placement_node* %node170, %placement_node** %struct_build173
  %struct_build174 = getelementptr inbounds %placement_node* %malloc172, i32 0, i32 1
  store %placement* %placed1168, %placement** %struct_build174
  store %placement_node* %malloc172, %placement_node** %add_plcmt169
  %frames175 = load %frame*** %frames
  %arr_access176 = getelementptr %frame** %frames175, i32 2
  %arr_access_val177 = load %frame** %arr_access176
  %placed2178 = load %placement** %placed2
  %add_plcmt179 = getelementptr inbounds %frame* %arr_access_val177, i32 0, i32 0
  %node180 = load %placement_node** %add_plcmt179
  %malloccall181 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc182 = bitcast i8* %malloccall181 to %placement_node*
  %struct_build183 = getelementptr inbounds %placement_node* %malloc182, i32 0, i32 0
  store %placement_node* %node180, %placement_node** %struct_build183
  %struct_build184 = getelementptr inbounds %placement_node* %malloc182, i32 0, i32 1
  store %placement* %placed2178, %placement** %struct_build184
  store %placement_node* %malloc182, %placement_node** %add_plcmt179
  %frames185 = load %frame*** %frames
  %arr_access186 = getelementptr %frame** %frames185, i32 3
  %arr_access_val187 = load %frame** %arr_access186
  %placed3188 = load %placement** %placed3
  %add_plcmt189 = getelementptr inbounds %frame* %arr_access_val187, i32 0, i32 0
  %node190 = load %placement_node** %add_plcmt189
  %malloccall191 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc192 = bitcast i8* %malloccall191 to %placement_node*
  %struct_build193 = getelementptr inbounds %placement_node* %malloc192, i32 0, i32 0
  store %placement_node* %node190, %placement_node** %struct_build193
  %struct_build194 = getelementptr inbounds %placement_node* %malloc192, i32 0, i32 1
  store %placement* %placed3188, %placement** %struct_build194
  store %placement_node* %malloc192, %placement_node** %add_plcmt189
  %frames195 = load %frame*** %frames
  %arr_access196 = getelementptr %frame** %frames195, i32 4
  %arr_access_val197 = load %frame** %arr_access196
  %placed4198 = load %placement** %placed4
  %add_plcmt199 = getelementptr inbounds %frame* %arr_access_val197, i32 0, i32 0
  %node200 = load %placement_node** %add_plcmt199
  %malloccall201 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc202 = bitcast i8* %malloccall201 to %placement_node*
  %struct_build203 = getelementptr inbounds %placement_node* %malloc202, i32 0, i32 0
  store %placement_node* %node200, %placement_node** %struct_build203
  %struct_build204 = getelementptr inbounds %placement_node* %malloc202, i32 0, i32 1
  store %placement* %placed4198, %placement** %struct_build204
  store %placement_node* %malloc202, %placement_node** %add_plcmt199
  %frames205 = load %frame*** %frames
  %arr_access206 = getelementptr %frame** %frames205, i32 5
  %arr_access_val207 = load %frame** %arr_access206
  %placed5208 = load %placement** %placed5
  %add_plcmt209 = getelementptr inbounds %frame* %arr_access_val207, i32 0, i32 0
  %node210 = load %placement_node** %add_plcmt209
  %malloccall211 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64), i64 2) to i32))
  %malloc212 = bitcast i8* %malloccall211 to %placement_node*
  %struct_build213 = getelementptr inbounds %placement_node* %malloc212, i32 0, i32 0
  store %placement_node* %node210, %placement_node** %struct_build213
  %struct_build214 = getelementptr inbounds %placement_node* %malloc212, i32 0, i32 1
  store %placement* %placed5208, %placement** %struct_build214
  store %placement_node* %malloc212, %placement_node** %add_plcmt209
  %frames215 = load %frame*** %frames
  %render = call i32 (i32, %frame**, i32, i32, i32, ...)* @render(i32 6, %frame** %frames215, i32 1, i32 800, i32 600)
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i32 %render)
  ret void
}

declare noalias i8* @malloc(i32)
