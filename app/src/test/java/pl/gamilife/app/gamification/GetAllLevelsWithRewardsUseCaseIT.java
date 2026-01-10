package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getalllevelwithrewards.GetAllLevelsWithRewardsCommand;
import pl.gamilife.gamification.application.usecase.getalllevelwithrewards.GetAllLevelsWithRewardsResult;
import pl.gamilife.gamification.application.usecase.getalllevelwithrewards.GetAllLevelsWithRewardsUseCase;

import static org.assertj.core.api.Assertions.assertThat;

class GetAllLevelsWithRewardsUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetAllLevelsWithRewardsUseCase getAllLevelsWithRewardsUseCase;

    @Test
    @DisplayName("Should return all levels")
    void shouldReturnAllLevels() {
        // when
        GetAllLevelsWithRewardsResult result = getAllLevelsWithRewardsUseCase.execute(new GetAllLevelsWithRewardsCommand());

        flushAndClear();

        // then
        assertThat(result.levels()).isNotEmpty();
        assertThat(result.levels()).anyMatch(level -> level.level() == 1 && level.requiredExperience() == 100);
    }
}