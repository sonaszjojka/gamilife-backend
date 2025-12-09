package pl.gamilife.task.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.task.domain.model.TaskDifficulty;

public interface JpaTaskDifficultyRepository extends JpaRepository<TaskDifficulty, Integer> {
}
