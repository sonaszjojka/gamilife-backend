package pl.gamilife.gamification.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

class UserAchievementTest {

    @Test
    void shouldCreateUserAchievement_whenValidDataIsProvided() {
        // given
        UUID userId = UUID.randomUUID();
        Achievement achievement = Instancio.create(Achievement.class);

        // when
        UserAchievement result = UserAchievement.create(userId, achievement);

        // then
        assertThat(result).isNotNull();
        assertThat(result.getUserId()).isEqualTo(userId);
        assertThat(result.getAchievement()).isEqualTo(achievement);
        assertThat(result.getAchievementId()).isEqualTo(achievement.getId());
    }

    @Test
    void shouldThrowException_whenUserIdIsNull() {
        // given
        UUID userId = null;
        Achievement achievement = Instancio.create(Achievement.class);

        // when
        Throwable thrown = catchThrowable(() -> UserAchievement.create(userId, achievement));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("User ID cannot be null");
    }

    @Test
    void shouldThrowException_whenAchievementIsNull() {
        // given
        UUID userId = UUID.randomUUID();
        Achievement achievement = null;

        // when
        Throwable thrown = catchThrowable(() -> UserAchievement.create(userId, achievement));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Achievement cannot be null");
    }

    @Test
    void shouldThrowException_whenUserIdAndAchievementAreNull() {
        // given
        UUID userId = null;
        Achievement achievement = null;

        // when
        Throwable thrown = catchThrowable(() -> UserAchievement.create(userId, achievement));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("User ID cannot be null");
    }
}
