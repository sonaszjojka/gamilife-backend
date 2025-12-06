package pl.gamilife.gamification.application.usecase.getstartinggamificationvalues;

import pl.gamilife.api.gamification.dto.StartingGamificationValuesDto;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface GetStartingGamificationValuesUseCase extends UseCase<GetStartingGamificationValuesCommand, StartingGamificationValuesDto> {
}
