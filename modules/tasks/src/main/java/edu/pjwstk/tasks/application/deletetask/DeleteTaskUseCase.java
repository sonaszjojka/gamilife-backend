package edu.pjwstk.tasks.application.deletetask;

import java.util.UUID;

public interface DeleteTaskUseCase {
    void execute(UUID taskId);
}
