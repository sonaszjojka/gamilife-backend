package pl.gamilife.task.repository;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.task.entity.TaskDifficulty;

import java.util.Optional;

public interface TaskDifficultyRepository {
    Optional<TaskDifficulty> findById(@NotNull(message = "Difficulty Id cannot be null") Integer difficultyId);
}
