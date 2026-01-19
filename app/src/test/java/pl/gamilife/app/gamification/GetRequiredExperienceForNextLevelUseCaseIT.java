package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getrequiredexperiencefornextlevel.GetRequiredExperienceForNextLevelCommand;
import pl.gamilife.gamification.application.usecase.getrequiredexperiencefornextlevel.GetRequiredExperienceForNextLevelUseCase;

import static org.assertj.core.api.Assertions.assertThat;

class GetRequiredExperienceForNextLevelUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetRequiredExperienceForNextLevelUseCase getRequiredExperienceForNextLevelUseCase;

    @Test
    @DisplayName("Should return required experience for next level")
    void shouldReturnRequiredExperience() {
        // when
        Integer result = getRequiredExperienceForNextLevelUseCase.execute(
                new GetRequiredExperienceForNextLevelCommand(1)
        );

        flushAndClear();

        // then
        assertThat(result).isEqualTo(200);
    }

    @Test
    @DisplayName("Should return null for max level")
    void shouldReturnNullForMaxLevel() {
        // when
        Integer result = getRequiredExperienceForNextLevelUseCase.execute(
                new GetRequiredExperienceForNextLevelCommand(100)
        );

        flushAndClear();

        // then
        assertThat(result).isNull();
    }
}