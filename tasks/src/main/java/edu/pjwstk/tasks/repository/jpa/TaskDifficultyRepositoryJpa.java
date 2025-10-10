package edu.pjwstk.tasks.repository.jpa;

import edu.pjwstk.tasks.domain.TaskCategory;
import edu.pjwstk.tasks.domain.TaskDifficulty;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskDifficultyRepositoryJpa extends JpaRepository<TaskDifficulty, Integer> {
}
