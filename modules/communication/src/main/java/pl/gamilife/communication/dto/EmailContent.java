package pl.gamilife.communication.dto;

import lombok.Builder;
import lombok.Value;

@Builder
@Value
public class EmailContent {
    String subject;
    String content;
    String contentType;
}
