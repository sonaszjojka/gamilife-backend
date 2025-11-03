package edu.pjwstk.groups.grouprequest.deletegrouprequest;


import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.groups.entity.*;
import edu.pjwstk.groups.repository.GroupRequestRepository;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestResponse;
import edu.pjwstk.groups.usecase.deletegrouprequest.DeleteGroupRequestUseCaseImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class DeleteGroupRequestTest {
    @Mock
    GroupRequestRepository groupRequestRepository;
    @Mock
    AuthApi authApi;
    @InjectMocks
    DeleteGroupRequestUseCaseImpl deleteGroupRequestUseCase;

    @Test
    void shouldDeleteGroupRequestSuccessfully() {

        UUID groupId = UUID.randomUUID();
        UUID userId = UUID.randomUUID();
        UUID adminId = UUID.randomUUID();

        GroupMember administrator = GroupMember.builder()
                .groupMemberId(UUID.randomUUID())
                .userId(adminId)
                .build();

        Group group = Group.builder()
                .groupId(groupId)
                .adminId(adminId)
                .groupType(GroupType.builder().groupTypeId(3).title("REQUEST_ONLY").build())
                .membersLimit(10)
                .groupMembers(List.of(administrator))
                .build();

        CurrentUserDto currentUser = new CurrentUserDto(userId, "test@example.com");

        GroupRequestStatus pendingStatus = GroupRequestStatus.builder()
                .groupRequestStatusId(1)
                .title("PENDING")
                .build();

        GroupRequest existingGroupRequest = GroupRequest.builder()
                .groupRequestId(UUID.randomUUID())
                .userId(userId)
                .groupRequested(group)
                .groupRequestStatus(pendingStatus)
                .createdAt(Instant.now())
                .build();

        UUID providedGroupRequestId = existingGroupRequest.getGroupRequestId();

        when(groupRequestRepository.findById(providedGroupRequestId))
                .thenReturn(java.util.Optional.of(existingGroupRequest));
        when(authApi.getCurrentUser())
                .thenReturn(java.util.Optional.of(currentUser));

        deleteGroupRequestUseCase.execute(providedGroupRequestId);


       verify(groupRequestRepository).findById(providedGroupRequestId);
       verify(authApi).getCurrentUser();
       verify(groupRequestRepository).deleteById(providedGroupRequestId);






    }







}
