import pandas as pd
import xgboost as xgb
import operator
from matplotlib import pylab as plt
from sklearn import preprocessing

# import data
train = pd.read_csv("train.csv")
test = pd.read_csv("test.csv")
sample = pd.read_csv('sample_submission.csv')

# drop ids and get labels
labels = train.target.values
labels = preprocessing.LabelEncoder().fit_transform(labels)
train = train.drop(["id", "target"], axis=1)
features=list(train.columns[0:])
test = test.drop("id", axis = 1)

# train xgboost classifier
params = {"objective": "multi:softprob", "eval_metric":"mlogloss", "num_class": 9, "max_depth": 12, "max_iterations": 4800,
          "min_child_weight": 4.9208250938262745, "row_subsample": .9134478530382129, "min_loss_reduction": .5132278416508804,
                  "column_subsample": .730128689911957, "step_size": .009, "verbosity": True}

train_xgb = xgb.DMatrix(train, labels)
test_xgb  = xgb.DMatrix(test)
trainRound = 100
gbm = xgb.train(params, train_xgb, trainRound)
prediction = gbm.predict(test_xgb)

# create a feature map
outfile = open('xgb.fmap', 'w')
i = 0
for feat in features:
    outfile.write('{0}\t{1}\tq\n'.format(i, feat))
    i = i + 1
outfile.close()

# plot feature importance
importance = gbm.get_fscore(fmap='xgb.fmap')
importance = sorted(importance.items(), key=operator.itemgetter(1))
df = pd.DataFrame(importance, columns=['feature', 'fscore'])
df['fscore'] = df['fscore'] / df['fscore'].sum()
plt.figure()
df.plot()
df.plot(kind='barh', x='feature', y='fscore', legend=False, figsize=(6, 20))
plt.title('XGBoost Feature Importance')
plt.xlabel('relative importance')
plt.gcf().savefig('feature_importance_xgb.png')

# create submission file
prediction = pd.DataFrame(prediction, index=sample.id.values, columns=sample.columns[1:])
prediction.to_csv('prediction_xg.csv', index_label='id')