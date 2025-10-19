package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.common.groupsApi.dto.GroupMemberDto;
import edu.pjwstk.groups.entity.GroupMember;
import org.springframework.stereotype.Component;

@Component
public class GroupMemberMapperImpl implements GroupMemberMapper {

    @Override
    public GroupMemberDto toResponse(GroupMember groupMember) {
        return GroupMemberDto.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .memberGroup(new GroupMemberDto.GroupDto(groupMember.getMemberGroup().getGroupId()))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}
