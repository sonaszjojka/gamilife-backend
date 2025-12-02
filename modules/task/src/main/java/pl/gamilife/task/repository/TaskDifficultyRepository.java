package pl.gamilife.task.repository;

import pl.gamilife.task.entity.TaskDifficulty;
import jakarta.validation.constraints.NotNull;

import java.util.Optional;

public interface TaskDifficultyRepository {
    Optional<TaskDifficulty> findById(@NotNull(message = "Difficulty Id cannot be null") Integer difficultyId);
}
