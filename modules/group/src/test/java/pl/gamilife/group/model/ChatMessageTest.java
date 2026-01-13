package pl.gamilife.group.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

class ChatMessageTest {

    @Test
    void shouldCreateChatMessage_whenValidDataIsProvided() {
        // given
        String content = "Hello team, let's raid!";
        boolean isImportant = true;

        Group group = Instancio.create(Group.class);
        GroupMember groupMember = Instancio.create(GroupMember.class);

        // when
        ChatMessage message = ChatMessage.create(content, isImportant, group, groupMember);

        // then
        assertThat(message).isNotNull();
        assertThat(message.getContent()).isEqualTo(content);
        assertThat(message.getIsImportant()).isTrue();
        assertThat(message.getGroup()).isEqualTo(group);
        assertThat(message.getGroupId()).isEqualTo(group.getId());
        assertThat(message.getGroupMember()).isEqualTo(groupMember);
        assertThat(message.getGroupMemberId()).isEqualTo(groupMember.getId());
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "   "})
    void shouldThrowException_whenContentIsInvalid(String invalidContent) {
        // given
        ChatMessage message = Instancio.create(ChatMessage.class);

        // when
        Throwable thrown = catchThrowable(() -> message.setContent(invalidContent));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Content cannot be null or empty");
    }

    @Test
    void shouldThrowException_whenContentIsNull() {
        // given
        ChatMessage message = Instancio.create(ChatMessage.class);

        // when
        Throwable thrown = catchThrowable(() -> message.setContent(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Content cannot be null or empty");
    }

    @Test
    void shouldThrowException_whenContentIsTooLong() {
        // given
        String longContent = "a".repeat(256);
        ChatMessage message = Instancio.create(ChatMessage.class);

        // when
        Throwable thrown = catchThrowable(() -> message.setContent(longContent));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Content cannot be longer than 255 characters");
    }

    @Test
    void shouldSetImportance_whenValueIsChanged() {
        // given
        ChatMessage message = Instancio.create(ChatMessage.class);
        boolean newImportance = !message.getIsImportant(); // Flip the value

        // when
        message.setImportance(newImportance);

        // then
        assertThat(message.getIsImportant()).isEqualTo(newImportance);
    }

    @Test
    void shouldSetGroup_whenGroupIsValid() {
        // given
        Group group = Instancio.create(Group.class);
        ChatMessage message = Instancio.create(ChatMessage.class);

        // when
        message.setGroup(group);

        // then
        assertThat(message.getGroup()).isEqualTo(group);
        assertThat(message.getGroupId()).isEqualTo(group.getId());
    }

    @Test
    void shouldThrowException_whenGroupIsNull() {
        // given
        ChatMessage message = Instancio.create(ChatMessage.class);

        // when
        Throwable thrown = catchThrowable(() -> message.setGroup(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group cannot be null");
    }

    @Test
    void shouldSetGroupMember_whenGroupMemberIsValid() {
        // given
        GroupMember groupMember = Instancio.create(GroupMember.class);
        ChatMessage message = Instancio.create(ChatMessage.class);

        // when
        message.setGroupMember(groupMember);

        // then
        assertThat(message.getGroupMember()).isEqualTo(groupMember);
        assertThat(message.getGroupMemberId()).isEqualTo(groupMember.getId());
    }

    @Test
    void shouldThrowException_whenGroupMemberIsNull() {
        // given
        ChatMessage message = Instancio.create(ChatMessage.class);

        // when
        Throwable thrown = catchThrowable(() -> message.setGroupMember(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group member cannot be null");
    }
}