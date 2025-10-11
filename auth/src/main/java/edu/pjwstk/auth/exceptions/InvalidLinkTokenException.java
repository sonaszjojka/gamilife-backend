package edu.pjwstk.auth.exceptions;

public class InvalidLinkTokenException extends RuntimeException {
  public InvalidLinkTokenException(String message) {
    super(message);
  }
}
