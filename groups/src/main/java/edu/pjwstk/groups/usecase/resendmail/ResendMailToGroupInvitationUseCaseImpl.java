package edu.pjwstk.groups.usecase.resendmail;

import edu.pjwstk.core.event.GroupInvitationCreatedEvent;
import edu.pjwstk.groups.exception.domain.GroupInvitationNotFoundException;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.repository.GroupInvitationJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class ResendMailToGroupInvitationUseCaseImpl implements ResendMailToGroupInvitationUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public Void execute(ResendMailToGroupInvitationCommand cmd) {
        GroupInvitation groupInvitation = getGroupInvitationWithGroup(cmd.groupId(), cmd.groupInvitationId());

        eventPublisher.publishEvent(new GroupInvitationCreatedEvent(
                groupInvitation.getUserId(),
                groupInvitation.getGroup().getJoinCode(),
                groupInvitation.getLink()
        ));

        return null;
    }

    private GroupInvitation getGroupInvitationWithGroup(UUID groupId, UUID groupInvitationId) {
        return groupInvitationRepository.findWithGroupByGroupInvitationIdAndGroupId(groupInvitationId, groupId)
                .orElseThrow(
                        () -> new GroupInvitationNotFoundException("Group invitation with id: " + groupInvitationId
                                + " not found!")
                );
    }
}
