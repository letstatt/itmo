import torch.nn as nn


class LSTM(nn.Module):
    def __init__(self, num_classes, input_size, hidden_size, num_layers):
        super(LSTM, self).__init__()
        self.lstm = nn.LSTM(input_size=input_size, hidden_size=hidden_size, num_layers=num_layers, dropout=0.2)
        self.fc = nn.Linear(num_layers * hidden_size, hidden_size)
        self.dropout = nn.Dropout(p=0.4)
        self.fc2 = nn.Linear(hidden_size, num_classes)

    def forward(self, input):
        _, (hidden, _) = self.lstm(input)
        out = self.fc(hidden.view(-1))
        out = self.dropout(out)
        out = self.fc2(out)
        return out

