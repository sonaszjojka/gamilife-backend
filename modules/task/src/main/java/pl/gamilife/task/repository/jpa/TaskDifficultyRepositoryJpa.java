package pl.gamilife.task.repository.jpa;

import pl.gamilife.task.entity.TaskDifficulty;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskDifficultyRepositoryJpa extends JpaRepository<TaskDifficulty, Integer> {
}
