package pl.gamilife.grouptask.usecase.deletegrouptask;

import java.util.UUID;

public interface DeleteGroupTaskUseCase {
    void execute(UUID userId, UUID groupTaskId);
}
