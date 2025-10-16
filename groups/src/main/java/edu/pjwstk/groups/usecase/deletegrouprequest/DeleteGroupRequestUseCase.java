package edu.pjwstk.groups.usecase.deletegrouprequest;

import java.util.UUID;

public interface DeleteGroupRequestUseCase {
    void execute(UUID groupRequestId);
}
