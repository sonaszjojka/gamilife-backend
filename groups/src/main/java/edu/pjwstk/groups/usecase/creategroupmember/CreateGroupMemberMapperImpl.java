package edu.pjwstk.groups.usecase.creategroupmember;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateGroupMemberMapperImpl implements CreateGroupMemberMapper {

    @Override
    public GroupMember toEntity(UUID userId, Group group, UUID groupMemberId) {
        return GroupMember.builder()
                .groupMemberId(groupMemberId)
                .memberGroup(group)
                .userId(userId)
                .leftAt(null)
                .totalEarnedMoney(0)
                .groupMoney(0)
                .build();
    }

    @Override
    public CreateGroupMemberResponse toResponse(GroupMember savedGroupMember) {
        if (savedGroupMember == null) {
            return null;
        }

        return CreateGroupMemberResponse.builder()
                .groupMemberId(savedGroupMember.getGroupMemberId())
                .memberGroup(
                        savedGroupMember.getMemberGroup() != null
                                ? CreateGroupMemberResponse.GroupDto.builder()
                                .groupId(savedGroupMember.getMemberGroup().getGroupId())
                                .adminId(savedGroupMember.getMemberGroup().getAdminId())
                                .build()
                                : null
                )
                .userId(savedGroupMember.getUserId())
                .joinedAt(savedGroupMember.getJoinedAt())
                .leftAt(savedGroupMember.getLeftAt())
                .groupMoney(savedGroupMember.getGroupMoney())
                .totalEarnedMoney(savedGroupMember.getTotalEarnedMoney())
                .build();
    }
}
