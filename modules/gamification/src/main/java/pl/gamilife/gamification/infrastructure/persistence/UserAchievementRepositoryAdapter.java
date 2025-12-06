package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.UserAchievement;
import pl.gamilife.gamification.domain.port.repository.UserAchievementRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaUserAchievementRepository;

@Repository
@AllArgsConstructor
public class UserAchievementRepositoryAdapter implements UserAchievementRepository {

    private final JpaUserAchievementRepository jpaUserAchievementRepository;

    @Override
    public void save(UserAchievement userAchievement) {
        jpaUserAchievementRepository.save(userAchievement);
    }
}
