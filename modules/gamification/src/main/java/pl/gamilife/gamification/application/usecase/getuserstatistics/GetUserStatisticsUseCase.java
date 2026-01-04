package pl.gamilife.gamification.application.usecase.getuserstatistics;

import pl.gamilife.shared.kernel.architecture.UseCase;

import java.util.List;

public interface GetUserStatisticsUseCase extends UseCase<GetUserStatisticsCommand, List<GetUserStatisticsResult>> {
    List<GetUserStatisticsResult> execute(GetUserStatisticsCommand getUserStatisticsCommand);
}
