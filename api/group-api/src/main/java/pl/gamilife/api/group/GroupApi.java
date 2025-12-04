package pl.gamilife.api.group;


import pl.gamilife.api.group.dto.FindAllGroupsByUserIdWhereUserIsMemberResult;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.api.group.dto.GroupMemberDto;

import java.util.UUID;

public interface GroupApi {

    GroupMemberDto findGroupMemberById(UUID groupMemberId);

    GroupDto findGroupById(UUID groupId);

    FindAllGroupsByUserIdWhereUserIsMemberResult findAllGroupsByUserIdWhereUserIsMember(UUID userId,
                                                                                        Integer page,
                                                                                        Integer size,
                                                                                        String joinCode,
                                                                                        Integer groupType,
                                                                                        String groupName);
}
