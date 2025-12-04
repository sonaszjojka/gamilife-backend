package pl.gamilife.task.repository.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.task.entity.TaskDifficulty;

public interface TaskDifficultyRepositoryJpa extends JpaRepository<TaskDifficulty, Integer> {
}
