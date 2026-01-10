package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getgamificationuser.GetGamificationUserCommand;
import pl.gamilife.gamification.application.usecase.getgamificationuser.GetGamificationUserResult;
import pl.gamilife.gamification.application.usecase.getgamificationuser.GetGamificationUserUseCase;
import pl.gamilife.user.persistence.User;

import static org.assertj.core.api.Assertions.assertThat;

class GetGamificationUserUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetGamificationUserUseCase getGamificationUserUseCase;

    @Test
    @DisplayName("Should return gamification user details")
    void shouldReturnGamificationUserDetails() {
        // given
        User user = createUserWithStats();

        // when
        GetGamificationUserResult result = getGamificationUserUseCase.execute(
                new GetGamificationUserCommand(user.getId())
        );

        flushAndClear();

        // then
        assertThat(result.userId()).isEqualTo(user.getId());
        assertThat(result.username()).isEqualTo(user.getUsername());
        assertThat(result.level()).isGreaterThanOrEqualTo(0);
        assertThat(result.experience()).isGreaterThanOrEqualTo(0);
        assertThat(result.money()).isGreaterThanOrEqualTo(0);
    }
}