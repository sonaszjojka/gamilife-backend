package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.UserAchievement;

public interface UserAchievementRepository {
    void save(UserAchievement userAchievement);
}
