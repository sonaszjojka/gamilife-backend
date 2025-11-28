package edu.pjwstk.api.groups;


import edu.pjwstk.api.groups.dto.FindAllGroupsByUserIdWhereUserIsMemberResult;
import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.api.groups.dto.GroupMemberDto;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

public interface GroupApi {

    @Transactional(readOnly = true)
    GroupMemberDto findGroupMemberById(UUID groupMemberId);

    @Transactional(readOnly = true)
    GroupDto findGroupById(UUID groupId);

    @Transactional(readOnly = true)
    FindAllGroupsByUserIdWhereUserIsMemberResult findAllGroupsByUserIdWhereUserIsMember(UUID userId,
                                                                                        Integer page,
                                                                                        Integer size,
                                                                                        String joinCode,
                                                                                        Integer groupType,
                                                                                        String groupName);
}
