import torch
import torchaudio


class AudioFeatureExtractor(torch.nn.Module):
    def __init__(self, sample_rate, n_lfcc, hidden_size, n_filter=128, n_fft=256, f_min=40, f_max=20000, win_length=100, hop_length=80):
        super().__init__()
        self.lfcc = torchaudio.transforms.LFCC(
            sample_rate=sample_rate,
            n_filter=n_filter,
            f_min=f_min,
            f_max=f_max,
            n_lfcc=n_lfcc,
             speckwargs={
                 "n_fft": n_fft,
                 "win_length": win_length,
                 "hop_length": hop_length,
             }
        )
        self.gru = torch.nn.GRU(input_size=n_lfcc, hidden_size=hidden_size)
        self.dropout = torch.nn.Dropout(p=0.5)
        self.dense2 = torch.nn.Linear(in_features=hidden_size, out_features=8)

    def wav2lfcc(self, wav):
        return torch.fmax(self.lfcc(wav.abs())[0].permute(1, 0), torch.tensor([0.])).log1p()

    def lfcc2emb(self, lfcc):
        hidden = self.gru(lfcc)[1].view(-1)
        output = self.dropout(hidden)
        output = self.dense2(output)
        norm = torch.functional.norm(output, dim=0)
        return output / norm
