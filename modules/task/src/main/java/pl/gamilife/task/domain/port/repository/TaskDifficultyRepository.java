package pl.gamilife.task.domain.port.repository;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.task.domain.model.TaskDifficulty;

import java.util.Optional;

public interface TaskDifficultyRepository {
    Optional<TaskDifficulty> findById(@NotNull(message = "Difficulty Id cannot be null") Integer difficultyId);
}
