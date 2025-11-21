package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.Achievement;
import edu.pjwstk.gamification.repository.query.AchievementDetailsDto;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface AchievementRepository extends JpaRepository<Achievement, UUID> {

    @Query("""
            SELECT new edu.pjwstk.gamification.repository.query.AchievementDetailsDto(
                a.id,
                a.name,
                a.description,
                a.imagePath,
                a.goal,
                CASE WHEN ua.id IS NOT NULL THEN true ELSE false END,
                CASE
                    WHEN ua.id IS NOT NULL THEN a.goal
                    ELSE COALESCE(us.count, 0)
                END
            )
            FROM Achievement a
            LEFT JOIN a.userAchievements ua ON ua.userId = :userId
            LEFT JOIN a.statisticType st
            LEFT JOIN st.userStatistics us ON us.userId = :userId
            ORDER BY ua.earnedAt DESC NULLS LAST, st.id, a.goal
            """)
    List<AchievementDetailsDto> findAllAchievementDetailsByUserId(@Param("userId") UUID userId);

    @Query("""
                SELECT a
                FROM Achievement a
                WHERE a.statisticTypeId = :statisticTypeId
                AND NOT EXISTS (SELECT 1 FROM a.userAchievements ua WHERE ua.userId = :userId)
                ORDER BY a.goal
            """)
    Optional<Achievement> findByStatisticTypeIdAndNotEarnedByUserId(
            @Param("statisticTypeId") Integer statisticTypeId,
            @Param("userId") UUID userId,
            PageRequest pageable
    );
}
