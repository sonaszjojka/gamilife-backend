package pl.gamilife.group.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.group.enums.InvitationStatusEnum;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class InvitationStatusTest {

    @Test
    void shouldReturnCorrectEnum_whenIdIsValid() {
        // given
        InvitationStatusEnum expectedEnum = InvitationStatusEnum.values()[0];
        InvitationStatus status = Instancio.of(InvitationStatus.class)
                .set(field(InvitationStatus::getId), expectedEnum.getId())
                .create();

        // when
        InvitationStatusEnum result = status.toEnum();

        // then
        assertThat(result).isEqualTo(expectedEnum);
    }

    @Test
    void shouldThrowException_whenIdIsInvalid() {
        // given
        int invalidId = Integer.MAX_VALUE;
        InvitationStatus status = Instancio.of(InvitationStatus.class)
                .set(field(InvitationStatus::getId), invalidId)
                .create();

        // when
        Throwable thrown = catchThrowable(status::toEnum);

        // then
        assertThat(thrown).isInstanceOf(RuntimeException.class);
    }

    @Test
    void shouldCreateInvitationStatus_withBuilderAndFields() {
        // given
        String title = "Pending";
        List<GroupInvitation> invitations = Instancio.createList(GroupInvitation.class);

        // when
        InvitationStatus status = InvitationStatus.builder()
                .title(title)
                .groupInvitations(invitations)
                .build();

        // then
        assertThat(status).isNotNull();
        assertThat(status.getTitle()).isEqualTo(title);
        assertThat(status.getGroupInvitations()).isEqualTo(invitations);
        assertThat(status.toString()).contains(title);
    }

    @Test
    void shouldSetAndGetProperties() {
        // given
        InvitationStatus status = new InvitationStatus();
        String title = "Accepted";
        List<GroupInvitation> invitations = Instancio.createList(GroupInvitation.class);

        // when
        status.setTitle(title);
        status.setGroupInvitations(invitations);

        // then
        assertThat(status.getTitle()).isEqualTo(title);
        assertThat(status.getGroupInvitations()).isEqualTo(invitations);
    }
}