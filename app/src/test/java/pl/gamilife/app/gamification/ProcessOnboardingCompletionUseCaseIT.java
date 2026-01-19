package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.processonboardingcompletion.ProcessOnboardingCompletionCommand;
import pl.gamilife.gamification.application.usecase.processonboardingcompletion.ProcessOnboardingCompletionUseCase;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

import static org.assertj.core.api.Assertions.assertThat;

class ProcessOnboardingCompletionUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessOnboardingCompletionUseCase processOnboardingCompletionUseCase;

    @Autowired
    private JpaUserRepository userRepository;

    @Test
    @DisplayName("Should reward user for onboarding completion")
    void shouldRewardUserForOnboardingCompletion() {
        // given
        User user = createUserWithStats();
        int initialExp = user.getExperience();

        // when
        processOnboardingCompletionUseCase.execute(new ProcessOnboardingCompletionCommand(user.getId()));

        flushAndClear();

        // then
        User updatedUser = userRepository.findById(user.getId()).orElseThrow();
        assertThat(updatedUser.getExperience()).isGreaterThan(initialExp);
        assertThat(updatedUser.getLevel()).isGreaterThan(0);
    }
}