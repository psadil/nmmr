vector<lower=0>[n_voxel] lengthOfMeanAngleVector = sqrt(rows_dot_self(meanAngleVector));
matrix[n_voxel, 2] meanAngleUnitVector = append_col(meanAngleVector[,1] ./ lengthOfMeanAngleVector,
                                                    meanAngleVector[,2] ./ lengthOfMeanAngleVector);
vector<lower = -pi(), upper = pi()>[n_voxel] meanAngle;

for(v in 1:n_voxel) meanAngle[v] = atan2(meanAngleUnitVector[v,2], meanAngleUnitVector[v,1]);
