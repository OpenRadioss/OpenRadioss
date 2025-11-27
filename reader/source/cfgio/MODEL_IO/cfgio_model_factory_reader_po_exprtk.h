#ifndef CFGIO_MODEL_FACTORY_READER_PO_EXPRTK_H
#define CFGIO_MODEL_FACTORY_READER_PO_EXPRTK_H

#include "mec_expression_evaluator_exprtk.h"
#include "solverCDR.h"

class ModelFactoryReaderPOExprTk : public ModelFactoryReaderPO
{
public:
    ModelFactoryReaderPOExprTk(vector<IMECPreObject*>* PreobjLst = nullptr) :
        ModelFactoryReaderPO(PreobjLst) {}

    virtual const IExpressionEvaluator* GetBaseExpressionEvaluator() const
    { return &myBaseExpressionEvaluator; }

private:
    ExpressionEvaluatorExprTk myBaseExpressionEvaluator;
};

#endif // CFGIO_MODEL_FACTORY_READER_PO_EXPRTK_H
