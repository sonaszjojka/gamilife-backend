package pl.gamilife.gamification.application.usecase.getstartinggamificationvalues;

import pl.gamilife.api.gamification.dto.GamificationValuesDto;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface GetStartingGamificationValuesUseCase extends UseCase<GetStartingGamificationValuesCommand, GamificationValuesDto> {
}
