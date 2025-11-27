package edu.pjwstk.tasks.repository.jpa;

import edu.pjwstk.tasks.entity.TaskDifficulty;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskDifficultyRepositoryJpa extends JpaRepository<TaskDifficulty, Integer> {
}
