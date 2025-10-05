package edu.pjwstk.tasks.deletetask;

import java.util.UUID;

public interface DeleteTaskUseCase {
    void execute(UUID taskId);
}
