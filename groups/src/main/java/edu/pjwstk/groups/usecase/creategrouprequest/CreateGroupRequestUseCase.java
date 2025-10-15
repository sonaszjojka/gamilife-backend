package edu.pjwstk.groups.usecase.creategrouprequest;

import jakarta.validation.Valid;

import java.util.UUID;

public interface CreateGroupRequestUseCase {
    CreateGroupRequestResponse execute(UUID groupId);
}
