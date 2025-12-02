package edu.pjwstk.tasks.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.tasks.exception.TaskErrorCode;

public class TaskDifficultyNotFoundException extends DomainException {
    public TaskDifficultyNotFoundException(String message) {
        super(TaskErrorCode.TASK_DIFFICULTY_NOT_FOUND, message);
    }
}
