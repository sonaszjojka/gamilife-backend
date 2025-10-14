package edu.pjwstk.groups.usecase.creategroup;

import jakarta.validation.Valid;

public interface CreateGroupUseCase {
    CreateGroupResponse execute(CreateGroupRequest request);
}
