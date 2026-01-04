package pl.gamilife.gamification.application.usecase.getuserstatistics;

import pl.gamilife.shared.kernel.architecture.UseCase;

public interface GetUserStatisticsUseCase extends UseCase<GetUserStatisticsCommand, GetUserStatisticsResult> {
    GetUserStatisticsResult execute(GetUserStatisticsCommand getUserStatisticsCommand);
}
