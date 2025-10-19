package edu.pjwstk.groups.usecase.creategroupmember.creategroupmemberafteracceptation;

import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import jakarta.validation.Valid;

public interface CreateGroupMemberAfterAcceptationUseCase {

    CreateGroupMemberResponse execute(@Valid CreateGroupMemberAfterAcceptationRequest request);

}
