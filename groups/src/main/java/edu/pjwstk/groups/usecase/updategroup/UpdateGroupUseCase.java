package edu.pjwstk.groups.usecase.updategroup;

import jakarta.validation.Valid;

import java.util.UUID;

public interface UpdateGroupUseCase {
    UpdateGroupResponse execute(UpdateGroupRequest request, UUID groupId);
}
